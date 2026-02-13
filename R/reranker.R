#' Advanced Reranking Module
#'
#' @description Learned weight reranking with BM25 + semantic fusion
#'
#' @name reranker
NULL

#' Advanced Reranker with Learned Weights
#'
#' @description Combines multiple signals for better reranking:
#' - Semantic similarity (word vectors)
#' - BM25/keyword overlap
#' - Query coverage
#' - Position bias
#' - Length normalization
#'
#' @export
AdvancedReranker <- R6::R6Class(
  "AdvancedReranker",
  public = list(
    #' @field weights Feature weights
    weights = NULL,

    #' @description Create a new AdvancedReranker
    #' @param semantic_weight Weight for semantic similarity (0-1)
    #' @param bm25_weight Weight for BM25 score (0-1)
    #' @param coverage_weight Weight for query term coverage (0-1)
    #' @param position_weight Weight for position bias (0-1)
    #' @param sentence_embedder Optional SentenceEmbedder for semantic scoring
    initialize = function(semantic_weight = 0.4,
                          bm25_weight = 0.3,
                          coverage_weight = 0.2,
                          position_weight = 0.1,
                          sentence_embedder = NULL) {

      self$weights <- list(
        semantic = semantic_weight,
        bm25 = bm25_weight,
        coverage = coverage_weight,
        position = position_weight
      )

      # Normalize weights
      total <- sum(unlist(self$weights))
      self$weights <- lapply(self$weights, function(w) w / total)

      private$sentence_embedder <- sentence_embedder
    },

    #' @description Set sentence embedder
    #' @param embedder SentenceEmbedder object
    set_embedder = function(embedder) {
      private$sentence_embedder <- embedder
      invisible(self)
    },

    #' @description Rerank results
    #' @param query Query text
    #' @param query_vector Query embedding vector
    #' @param results List of result objects with id, text, score
    #' @param doc_vectors Matrix of document vectors (optional)
    #' @param limit Number of results to return
    #' @return Reranked list of results
    rerank = function(query, query_vector = NULL, results, doc_vectors = NULL, limit = 10) {

      if (length(results) == 0) return(results)

      n <- length(results)
      scores <- matrix(0, nrow = n, ncol = 4)
      colnames(scores) <- c("semantic", "bm25", "coverage", "position")

      # Extract texts
      texts <- sapply(results, function(r) r$text %||% "")
      original_scores <- sapply(results, function(r) r$score %||% 0)

      # 1. Semantic similarity
      if (!is.null(private$sentence_embedder) && !is.null(query_vector)) {
        doc_vecs <- private$sentence_embedder$embed(texts)
        scores[, "semantic"] <- private$compute_semantic_scores(query_vector, doc_vecs)
      } else if (!is.null(doc_vectors) && !is.null(query_vector)) {
        scores[, "semantic"] <- private$compute_semantic_scores(query_vector, doc_vectors)
      } else {
        # Fallback: use original scores
        scores[, "semantic"] <- private$normalize_scores(original_scores)
      }

      # 2. BM25/keyword scores
      scores[, "bm25"] <- private$compute_bm25_scores(query, texts)

      # 3. Query coverage (what % of query terms appear in doc)
      scores[, "coverage"] <- private$compute_coverage_scores(query, texts)

      # 4. Position bias (earlier results get slight boost)
      scores[, "position"] <- private$compute_position_scores(n)

      # Combine scores with weights
      final_scores <- scores[, "semantic"] * self$weights$semantic +
        scores[, "bm25"] * self$weights$bm25 +
        scores[, "coverage"] * self$weights$coverage +
        scores[, "position"] * self$weights$position

      # Rerank
      order_idx <- order(final_scores, decreasing = TRUE)[1:min(limit, n)]

      # Return reranked results
      lapply(order_idx, function(i) {
        r <- results[[i]]
        r$score <- final_scores[i]
        r$rerank_details <- list(
          semantic = scores[i, "semantic"],
          bm25 = scores[i, "bm25"],
          coverage = scores[i, "coverage"],
          position = scores[i, "position"]
        )
        r
      })
    },

    #' @description Learn optimal weights from relevance judgments
    #' @param queries Character vector of queries
    #' @param results_list List of result lists (one per query)
    #' @param relevance_list List of relevance scores (1=relevant, 0=not)
    #' @param iterations Number of optimization iterations
    learn_weights = function(queries, results_list, relevance_list, iterations = 100) {

      message("Learning optimal weights...")

      best_weights <- self$weights
      best_score <- 0

      # Grid search (simple but effective)
      weight_options <- seq(0.1, 0.5, by = 0.1)

      for (iter in seq_len(iterations)) {
        # Random weights
        w <- runif(4)
        w <- w / sum(w)

        test_weights <- list(
          semantic = w[1],
          bm25 = w[2],
          coverage = w[3],
          position = w[4]
        )

        # Evaluate
        score <- private$evaluate_weights(test_weights, queries, results_list, relevance_list)

        if (score > best_score) {
          best_score <- score
          best_weights <- test_weights
          message(sprintf("  Iter %d: NDCG@10 = %.4f", iter, score))
        }
      }

      self$weights <- best_weights
      message(sprintf("Optimal weights: semantic=%.2f, bm25=%.2f, coverage=%.2f, position=%.2f",
                      best_weights$semantic, best_weights$bm25,
                      best_weights$coverage, best_weights$position))

      invisible(self)
    }
  ),

  private = list(
    sentence_embedder = NULL,

    compute_semantic_scores = function(query_vec, doc_vecs) {
      if (is.null(query_vec) || is.null(doc_vecs)) {
        return(rep(0, nrow(doc_vecs)))
      }

      # Cosine similarity
      if (is.vector(query_vec)) {
        query_vec <- matrix(query_vec, nrow = 1)
      }

      query_norm <- sqrt(sum(query_vec^2))
      if (isTRUE(query_norm == 0) || !is.finite(query_norm)) return(rep(0, nrow(doc_vecs)))

      doc_norms <- sqrt(rowSums(doc_vecs^2))
      doc_norms[doc_norms == 0] <- 1

      sims <- as.vector(doc_vecs %*% t(query_vec)) / (doc_norms * query_norm)
      private$normalize_scores(sims)
    },

    compute_bm25_scores = function(query, texts) {
      query_tokens <- private$tokenize(query)
      if (length(query_tokens) == 0) return(rep(0, length(texts)))

      # BM25 parameters
      k1 <- 1.2
      b <- 0.75

      # Compute average document length
      doc_lengths <- sapply(texts, function(t) length(private$tokenize(t)))
      avg_dl <- mean(doc_lengths)

      scores <- sapply(seq_along(texts), function(i) {
        doc_tokens <- private$tokenize(texts[i])
        if (length(doc_tokens) == 0) return(0)

        dl <- doc_lengths[i]

        # Term frequencies in document
        tf <- sapply(query_tokens, function(qt) sum(doc_tokens == qt))

        # BM25 formula
        score <- sum(
          (tf * (k1 + 1)) / (tf + k1 * (1 - b + b * dl / avg_dl))
        )
        score / length(query_tokens)  # Normalize by query length
      })

      private$normalize_scores(scores)
    },

    compute_coverage_scores = function(query, texts) {
      query_tokens <- unique(private$tokenize(query))
      if (length(query_tokens) == 0) return(rep(0, length(texts)))

      sapply(texts, function(text) {
        doc_tokens <- private$tokenize(text)
        if (length(doc_tokens) == 0) return(0)
        sum(query_tokens %in% doc_tokens) / length(query_tokens)
      })
    },

    compute_position_scores = function(n) {
      # Slight position bias (earlier is better, but not by much)
      1 / log2(seq_len(n) + 1)
    },

    normalize_scores = function(scores) {
      if (length(scores) == 0) return(scores)
      min_s <- min(scores, na.rm = TRUE)
      max_s <- max(scores, na.rm = TRUE)
      if (isTRUE(max_s == min_s) || !is.finite(max_s) || !is.finite(min_s)) return(rep(0.5, length(scores)))
      (scores - min_s) / (max_s - min_s)
    },

    tokenize = function(text) {
      if (is.null(text) || text == "") return(character(0))
      tokens <- tolower(text)
      tokens <- gsub("[^a-z0-9 ]", " ", tokens)
      tokens <- strsplit(tokens, "\\s+")[[1]]
      tokens[nchar(tokens) > 1]
    },

    evaluate_weights = function(weights, queries, results_list, relevance_list) {
      # Compute NDCG@10 for each query and average
      ndcg_scores <- sapply(seq_along(queries), function(i) {
        query <- queries[i]
        results <- results_list[[i]]
        relevance <- relevance_list[[i]]

        # Temporarily use these weights
        old_weights <- self$weights
        self$weights <- weights

        # Rerank
        reranked <- self$rerank(query, NULL, results, NULL, 10)
        reranked_ids <- sapply(reranked, function(r) r$id)

        # Compute NDCG
        dcg <- 0
        for (j in seq_along(reranked_ids)) {
          id <- reranked_ids[j]
          rel <- relevance[[id]] %||% 0
          dcg <- dcg + rel / log2(j + 1)
        }

        # Ideal DCG
        sorted_rel <- sort(unlist(relevance), decreasing = TRUE)[1:10]
        idcg <- sum(sorted_rel / log2(seq_along(sorted_rel) + 1))

        self$weights <- old_weights

        if (idcg == 0) return(0)
        dcg / idcg
      })

      mean(ndcg_scores, na.rm = TRUE)
    }
  )
)

#' Maximal Marginal Relevance (MMR) Reranker
#'
#' @description Reranks for diversity using MMR algorithm
#'
#' @export
MMRReranker <- R6::R6Class(
  "MMRReranker",
  public = list(
    #' @field lambda Balance between relevance and diversity (0-1)
    lambda = 0.7,

    #' @description Create a new MMRReranker
    #' @param lambda Relevance vs diversity tradeoff (higher = more relevance)
    initialize = function(lambda = 0.7) {
      self$lambda <- lambda
    },

    #' @description Rerank for diversity
    #' @param query_vector Query embedding
    #' @param doc_vectors Matrix of document embeddings
    #' @param doc_ids Vector of document IDs
    #' @param scores Original relevance scores
    #' @param limit Number of results
    #' @return Data frame with reranked results
    rerank = function(query_vector, doc_vectors, doc_ids, scores, limit = 10) {
      if (is.vector(doc_vectors)) {
        doc_vectors <- matrix(doc_vectors, nrow = 1)
      }

      n <- length(doc_ids)
      limit <- min(limit, n)

      # Normalize scores to 0-1
      if (max(scores) > 0) {
        scores <- scores / max(scores)
      }

      selected <- c()
      selected_vectors <- matrix(nrow = 0, ncol = ncol(doc_vectors))
      remaining <- seq_len(n)

      for (i in seq_len(limit)) {
        if (length(remaining) == 0) break

        mmr_scores <- sapply(remaining, function(idx) {
          relevance <- scores[idx]

          if (nrow(selected_vectors) == 0) {
            diversity <- 0
          } else {
            # Max similarity to any selected document
            sims <- private$cosine_sim(doc_vectors[idx, ], selected_vectors)
            diversity <- max(sims)
          }

          self$lambda * relevance - (1 - self$lambda) * diversity
        })

        best_idx <- remaining[which.max(mmr_scores)]
        selected <- c(selected, best_idx)
        selected_vectors <- rbind(selected_vectors, doc_vectors[best_idx, ])
        remaining <- setdiff(remaining, best_idx)
      }

      data.frame(
        id = doc_ids[selected],
        score = scores[selected],
        mmr_rank = seq_along(selected),
        stringsAsFactors = FALSE
      )
    }
  ),

  private = list(
    cosine_sim = function(vec, matrix) {
      if (is.vector(matrix)) {
        matrix <- matrix(matrix, nrow = 1)
      }
      vec_norm <- sqrt(sum(vec^2))
      mat_norms <- sqrt(rowSums(matrix^2))
      mat_norms[mat_norms == 0] <- 1
      if (vec_norm == 0) return(rep(0, nrow(matrix)))
      as.vector(matrix %*% vec) / (mat_norms * vec_norm)
    }
  )
)

#' Null coalescing for reranker.R
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a
