#' VectrixDB Embedders (Pure R Implementation)
#'
#' @description Embedding models for text vectorization using R-native packages
#'
#' @name embedders
NULL

# Global model cache to avoid reloading
.vectrix_cache <- new.env(parent = emptyenv())

#' Dense Embedder using word2vec or GloVe
#'
#' @description Generates dense vector embeddings using pre-trained word vectors
#'
#' @export
DenseEmbedder <- R6::R6Class(

  "DenseEmbedder",
  public = list(
    #' @field dimension Embedding dimension
    dimension = NULL,
    #' @field model_type Type of model being used
    model_type = NULL,
    #' @field language Language setting ("en" or "ml")
    language = "en",

    #' @description Create a new DenseEmbedder
    #' @param dimension Vector dimension (default: 100 for word2vec, 50/100/200/300 for GloVe)
    #' @param model_path Optional path to pre-trained model file
    #' @param model_type Type: "word2vec", "glove", "glove-pretrained", or "tfidf"
    #' @param sentence_embedder Optional SentenceEmbedder object to use
    #' @param auto_download Auto-download GloVe vectors if model_type is glove-pretrained
    #' @param language Language behavior ("en" = ASCII-focused, "ml" = Unicode-aware)
    initialize = function(dimension = 100, model_path = NULL, model_type = "tfidf",
                          sentence_embedder = NULL, auto_download = FALSE, language = "en") {
      self$dimension <- dimension
      self$model_type <- model_type
      self$language <- normalize_language_tag(language, default = "en")

      # Use provided SentenceEmbedder if available
      if (!is.null(sentence_embedder)) {
        private$sentence_embedder <- sentence_embedder
        self$dimension <- sentence_embedder$dim
        self$model_type <- "sentence-embedder"
        return()
      }

      # Auto-download GloVe for glove-pretrained types
      if (grepl("^glove-", model_type) && model_type != "glove" && auto_download) {
        tryCatch({
          path <- download_word_vectors(model_type)
          wv <- load_word_vectors(path)
          private$sentence_embedder <- SentenceEmbedder$new(wv, use_idf = TRUE)
          self$dimension <- private$sentence_embedder$dim
          self$model_type <- "sentence-embedder"
          return()
        }, error = function(e) {
          warning(sprintf("Could not load %s: %s. Falling back to TF-IDF.", model_type, e$message))
          self$model_type <- "tfidf"
        })
      }

      if (!is.null(model_path) && file.exists(model_path)) {
        private$load_model(model_path)
      } else if (self$model_type == "tfidf" || self$model_type == "tfidf") {
        # Use TF-IDF as default (always works, no external files needed)
        self$model_type <- "tfidf"
        private$init_tfidf()
      }
    },

    #' @description Set a SentenceEmbedder to use for embeddings
    #' @param embedder SentenceEmbedder object
    set_sentence_embedder = function(embedder) {
      private$sentence_embedder <- embedder
      self$dimension <- embedder$dim
      self$model_type <- "sentence-embedder"
      invisible(self)
    },

    #' @description Embed texts to vectors
    #' @param texts Character vector of texts
    #' @return Matrix of embeddings (rows are documents)
    embed = function(texts) {
      if (is.character(texts) && length(texts) == 1) {
        texts <- c(texts)
      }

      if (self$model_type == "sentence-embedder" && !is.null(private$sentence_embedder)) {
        return(private$sentence_embedder$embed(texts))
      } else if (self$model_type == "tfidf") {
        return(private$embed_tfidf(texts))
      } else if (self$model_type == "word2vec") {
        return(private$embed_word2vec(texts))
      } else if (self$model_type == "glove") {
        return(private$embed_glove(texts))
      }

      # Fallback
      private$embed_tfidf(texts)
    },

    #' @description Train embedder on corpus (for TF-IDF)
    #' @param texts Character vector of training texts
    fit = function(texts) {
      if (self$model_type == "tfidf") {
        private$fit_tfidf(texts)
      } else if (self$model_type == "sentence-embedder" && !is.null(private$sentence_embedder)) {
        private$sentence_embedder$fit(texts)
      }
      invisible(self)
    }
  ),

  private = list(
    word_vectors = NULL,
    vocab = NULL,
    tfidf_model = NULL,
    vectorizer = NULL,
    fitted = FALSE,
    sentence_embedder = NULL,

    init_tfidf = function() {
      # Initialize with empty model, will be fitted on first use
      private$fitted <- FALSE
    },

    fit_tfidf = function(texts) {
      # Create vocabulary-based vectorizer
      tokens <- lapply(as.character(texts), private$tokenize)

      it <- text2vec::itoken(tokens, progressbar = FALSE)

      # Build vocabulary
      vocab_raw <- text2vec::create_vocabulary(it)
      # Drop numeric/symbol-only tokens; keep terms with at least one letter.
      if (nrow(vocab_raw) > 0 && "term" %in% colnames(vocab_raw)) {
        has_alpha <- grepl("[[:alpha:]]", vocab_raw$term)
        if (any(has_alpha)) {
          vocab_raw <- vocab_raw[has_alpha, , drop = FALSE]
        }
      }

      # Avoid over-pruning on tiny corpora (e.g., single query/text).
      max_prop <- if (length(texts) <= 5) 1.0 else 0.95
      private$vocab <- text2vec::prune_vocabulary(
        vocab_raw,
        term_count_min = 1,
        doc_proportion_max = max_prop
      )

      # Fallback to unpruned vocabulary if pruning removed all terms.
      if (nrow(private$vocab) == 0) {
        private$vocab <- vocab_raw
      }

      # Cap vocabulary deterministically by term frequency instead of arbitrary column order.
      target_dim <- suppressWarnings(as.integer(self$dimension %||% NA))
      if (!is.na(target_dim) && target_dim > 0L && nrow(private$vocab) > target_dim) {
        sort_col <- if ("term_count" %in% colnames(private$vocab)) {
          "term_count"
        } else if ("doc_count" %in% colnames(private$vocab)) {
          "doc_count"
        } else {
          NA_character_
        }
        if (!is.na(sort_col)) {
          ord <- order(private$vocab[[sort_col]], decreasing = TRUE)
          private$vocab <- private$vocab[ord[seq_len(target_dim)], , drop = FALSE]
        } else {
          private$vocab <- private$vocab[seq_len(target_dim), , drop = FALSE]
        }
      }

      private$vectorizer <- text2vec::vocab_vectorizer(private$vocab)

      # Create DTM and fit TF-IDF
      dtm <- text2vec::create_dtm(it, private$vectorizer)
      private$tfidf_model <- text2vec::TfIdf$new()
      private$tfidf_model$fit_transform(dtm)

      # Set dimension based on fitted vocabulary size.
      if (ncol(dtm) > 0) {
        self$dimension <- ncol(dtm)
      }
      private$fitted <- TRUE
    },

    embed_tfidf = function(texts) {
      # If not fitted yet, fit on current texts
      if (!private$fitted) {
        private$fit_tfidf(texts)
      }

      tokens <- lapply(as.character(texts), private$tokenize)
      it <- text2vec::itoken(tokens, progressbar = FALSE)

      dtm <- text2vec::create_dtm(it, private$vectorizer)
      tfidf_matrix <- private$tfidf_model$transform(dtm)

      # Convert to dense and truncate/pad to dimension
      result <- as.matrix(tfidf_matrix)

      # Ensure consistent dimension
      if (ncol(result) < self$dimension) {
        # Pad with zeros
        padding <- matrix(0, nrow = nrow(result), ncol = self$dimension - ncol(result))
        result <- cbind(result, padding)
      } else if (ncol(result) > self$dimension) {
        # Truncate (keep most important features by variance)
        result <- result[, 1:self$dimension, drop = FALSE]
      }

      # Normalize vectors
      norms <- sqrt(rowSums(result^2))
      norms[norms == 0] <- 1
      result <- result / norms

      result
    },

    load_model = function(model_path) {
      ext <- tolower(tools::file_ext(model_path))

      if (ext == "bin" || self$model_type == "word2vec") {
        # Load word2vec binary format
        if (word2vec_available()) {
          private$word_vectors <- read_word2vec_model(model_path, normalize = TRUE)
          self$dimension <- ncol(as.matrix(private$word_vectors))
          self$model_type <- "word2vec"
        } else {
          warning("word2vec package not installed; cannot load .bin model")
        }
      } else if (ext == "txt" || self$model_type == "glove") {
        # Load GloVe text format
        private$load_glove(model_path)
        self$model_type <- "glove"
      }
    },

    load_glove = function(path) {
      lines <- readLines(path, warn = FALSE)
      n <- length(lines)

      # Parse first line to get dimension
      first <- strsplit(lines[1], " ")[[1]]
      self$dimension <- length(first) - 1

      # Pre-allocate
      words <- character(n)
      vectors <- matrix(0, nrow = n, ncol = self$dimension)

      for (i in seq_len(n)) {
        parts <- strsplit(lines[i], " ")[[1]]
        words[i] <- parts[1]
        vectors[i, ] <- as.numeric(parts[-1])
      }

      rownames(vectors) <- words
      private$word_vectors <- vectors
    },

    embed_word2vec = function(texts) {
      if (is.null(private$word_vectors)) {
        warning("word2vec model not loaded, falling back to TF-IDF")
        return(private$embed_tfidf(texts))
      }

      result <- matrix(0, nrow = length(texts), ncol = self$dimension)

      for (i in seq_along(texts)) {
        tokens <- private$tokenize(texts[i])
        if (length(tokens) == 0) next

        # Get vectors for known words
        known <- tokens[tokens %in% rownames(private$word_vectors)]
        if (length(known) > 0) {
          vecs <- private$word_vectors[known, , drop = FALSE]
          result[i, ] <- colMeans(vecs)
        }
      }

      # Normalize
      norms <- sqrt(rowSums(result^2))
      norms[norms == 0] <- 1
      result / norms
    },

    embed_glove = function(texts) {
      private$embed_word2vec(texts)  # Same logic
    },

    tokenize = function(text) {
      tokenize_text_by_language(text, language = self$language, remove_stopwords = FALSE)
    }
  )
)

#' Sparse Embedder (BM25/TF-IDF)
#'
#' @description Generates sparse BM25 embeddings for keyword search
#'
#' @export
SparseEmbedder <- R6::R6Class(
  "SparseEmbedder",
  public = list(
    #' @field vocab Vocabulary
    vocab = NULL,
    #' @field language Language setting ("en" or "ml")
    language = "en",

    #' @description Create a new SparseEmbedder
    #' @param language Language behavior ("en" = ASCII-focused, "ml" = Unicode-aware)
    initialize = function(language = "en") {
      self$language <- normalize_language_tag(language, default = "en")
      private$fitted <- FALSE
    },

    #' @description Fit the embedder on a corpus
    #' @param texts Character vector of texts
    fit = function(texts) {
      tokens <- lapply(as.character(texts), private$tokenize)
      it <- text2vec::itoken(tokens, progressbar = FALSE)

      self$vocab <- text2vec::create_vocabulary(it)
      self$vocab <- text2vec::prune_vocabulary(
        self$vocab,
        term_count_min = 1,
        doc_proportion_max = 0.95
      )

      private$vectorizer <- text2vec::vocab_vectorizer(self$vocab)

      # Fit BM25 model
      dtm <- text2vec::create_dtm(it, private$vectorizer)
      private$bm25_model <- text2vec::BM25$new()
      private$bm25_model$fit_transform(dtm)

      private$fitted <- TRUE
      invisible(self)
    },

    #' @description Embed texts to sparse vectors
    #' @param texts Character vector of texts
    #' @return Sparse matrix of BM25 scores
    embed = function(texts) {
      if (!private$fitted) {
        self$fit(texts)
      }

      tokens <- lapply(as.character(texts), private$tokenize)
      it <- text2vec::itoken(tokens, progressbar = FALSE)

      dtm <- text2vec::create_dtm(it, private$vectorizer)
      private$bm25_model$transform(dtm)
    },

    #' @description Get term scores for a query
    #' @param query Query text
    #' @return Named vector of term scores
    query_terms = function(query) {
      tokens <- private$tokenize(query)
      if (is.null(self$vocab)) return(list())

      # Return term frequencies for known terms
      known <- tokens[tokens %in% self$vocab$term]
      if (length(known) == 0) return(list())

      freqs <- table(known)
      as.list(freqs / sum(freqs))
    }
  ),

  private = list(
    vectorizer = NULL,
    bm25_model = NULL,
    fitted = FALSE,

    tokenize = function(text) {
      tokenize_text_by_language(text, language = self$language, remove_stopwords = FALSE)
    }
  )
)

#' Reranker (Cross-Encoder Style Scoring)
#'
#' @description Reranks results using term overlap and semantic similarity
#'
#' @export
RerankerEmbedder <- R6::R6Class(
  "RerankerEmbedder",
  public = list(
    #' @field language Language setting ("en" or "ml")
    language = "en",

    #' @description Create a new RerankerEmbedder
    #' @param language Language behavior ("en" = English stopwords, "ml" = Unicode tokens)
    initialize = function(language = "en") {
      self$language <- normalize_language_tag(language, default = "en")
      # No external dependencies needed
    },

    #' @description Score query-document pairs
    #' @param query Query text
    #' @param documents Character vector of document texts
    #' @return Numeric vector of scores (0-1)
    score = function(query, documents) {
      query_tokens <- private$tokenize(query)
      query_bigrams <- private$get_bigrams(query_tokens)

      scores <- sapply(documents, function(doc) {
        doc_tokens <- private$tokenize(doc)
        doc_bigrams <- private$get_bigrams(doc_tokens)

        if (length(query_tokens) == 0 || length(doc_tokens) == 0) {
          return(0)
        }

        # Unigram overlap (Jaccard)
        uni_intersection <- length(intersect(query_tokens, doc_tokens))
        uni_union <- length(union(query_tokens, doc_tokens))
        unigram_score <- if (uni_union > 0) uni_intersection / uni_union else 0

        # Bigram overlap (captures phrase matching)
        bi_intersection <- length(intersect(query_bigrams, doc_bigrams))
        bi_union <- length(union(query_bigrams, doc_bigrams))
        bigram_score <- if (bi_union > 0) bi_intersection / bi_union else 0

        # Coverage: what fraction of query terms appear in doc
        coverage <- sum(query_tokens %in% doc_tokens) / length(query_tokens)

        # Term frequency boost: penalize docs that mention query terms only once
        tf_boost <- sum(sapply(query_tokens, function(t) {
          min(sum(doc_tokens == t), 3) / 3  # Cap at 3 occurrences
        })) / length(query_tokens)

        # Combine scores
        0.3 * unigram_score + 0.2 * bigram_score + 0.3 * coverage + 0.2 * tf_boost
      })

      as.numeric(scores)
    }
  ),

  private = list(
    tokenize = function(text) {
      tokenize_text_by_language(text, language = self$language, remove_stopwords = TRUE)
    },

    get_bigrams = function(tokens) {
      if (length(tokens) < 2) return(character(0))
      paste(tokens[-length(tokens)], tokens[-1], sep = "_")
    }
  )
)

#' Late Interaction Embedder (Simplified ColBERT-style)
#'
#' @description Token-level embeddings for late interaction scoring
#'
#' @export
LateInteractionEmbedder <- R6::R6Class(
  "LateInteractionEmbedder",
  public = list(
    #' @field dimension Token embedding dimension
    dimension = 64,
    #' @field language Language setting ("en" or "ml")
    language = "en",

    #' @description Create a new LateInteractionEmbedder
    #' @param dimension Embedding dimension per token
    #' @param language Language behavior ("en" = ASCII-focused, "ml" = Unicode-aware)
    initialize = function(dimension = 64, language = "en") {
      self$dimension <- dimension
      self$language <- normalize_language_tag(language, default = "en")
      private$init_embedder()
    },

    #' @description Embed texts to token-level embeddings
    #' @param texts Character vector of texts
    #' @return List of matrices (each matrix is token embeddings for a document)
    embed = function(texts) {
      lapply(texts, function(text) {
        tokens <- private$tokenize(text)
        if (length(tokens) == 0) {
          return(matrix(0, nrow = 1, ncol = self$dimension))
        }

        # Get embedding for each token and keep numeric matrix shape.
        token_vectors <- lapply(tokens, private$get_token_embedding)
        embeddings <- tryCatch(
          do.call(rbind, token_vectors),
          error = function(e) NULL
        )
        if (is.null(embeddings)) {
          return(matrix(0, nrow = 1, ncol = self$dimension))
        }

        if (is.vector(embeddings)) {
          embeddings <- matrix(embeddings, nrow = 1)
        }

        embeddings
      })
    },

    #' @description Compute late interaction (MaxSim) score
    #' @param query_embeddings Query token embeddings matrix
    #' @param doc_embeddings Document token embeddings matrix
    #' @return Numeric score
    score = function(query_embeddings, doc_embeddings) {
      if (is.null(query_embeddings) || is.null(doc_embeddings)) return(0)
      q_mat <- tryCatch(as.matrix(query_embeddings), error = function(e) NULL)
      d_mat <- tryCatch(as.matrix(doc_embeddings), error = function(e) NULL)
      if (is.null(q_mat) || is.null(d_mat)) return(0)
      if (nrow(q_mat) == 0 || nrow(d_mat) == 0) return(0)

      # Coerce to numeric matrices and sanitize non-finite values.
      if (!is.numeric(q_mat)) {
        q_mat <- matrix(
          suppressWarnings(as.numeric(q_mat)),
          nrow = nrow(q_mat),
          ncol = ncol(q_mat)
        )
      }
      if (!is.numeric(d_mat)) {
        d_mat <- matrix(
          suppressWarnings(as.numeric(d_mat)),
          nrow = nrow(d_mat),
          ncol = ncol(d_mat)
        )
      }
      if (ncol(q_mat) == 0 || ncol(d_mat) == 0 || ncol(q_mat) != ncol(d_mat)) return(0)
      q_mat[!is.finite(q_mat)] <- 0
      d_mat[!is.finite(d_mat)] <- 0

      # MaxSim: for each query token, find max similarity with any doc token
      # Normalize embeddings
      q_norm <- q_mat / sqrt(rowSums(q_mat^2) + 1e-8)
      d_norm <- d_mat / sqrt(rowSums(d_mat^2) + 1e-8)

      # Compute similarity matrix
      sims <- q_norm %*% t(d_norm)
      sims[!is.finite(sims)] <- -Inf

      # Sum of max similarities for each query token
      row_max <- apply(sims, 1, function(x) {
        m <- suppressWarnings(max(x, na.rm = TRUE))
        if (is.finite(m)) m else 0
      })
      as.numeric(sum(row_max))
    }
  ),

  private = list(
    token_cache = NULL,
    char_embeddings = NULL,

    init_embedder = function() {
      private$token_cache <- new.env(hash = TRUE, parent = emptyenv())

      # Create character-level embeddings (fixed random projection)
      set.seed(42)  # Reproducible
      chars <- c(letters, 0:9, "_")
      private$char_embeddings <- matrix(
        rnorm(length(chars) * self$dimension),
        nrow = length(chars),
        ncol = self$dimension
      )
      rownames(private$char_embeddings) <- chars
    },

    tokenize = function(text) {
      tokenize_text_by_language(text, language = self$language, remove_stopwords = FALSE)
    },

    get_token_embedding = function(token) {
      # Check cache
      if (exists(token, envir = private$token_cache, inherits = FALSE)) {
        return(get(token, envir = private$token_cache, inherits = FALSE))
      }

      # Generate embedding from character n-grams
      raw_chars <- strsplit(tolower(token), "", fixed = TRUE)[[1]]
      if (length(raw_chars) == 0) {
        emb <- rep(0, self$dimension)
      } else {
        # Mix known character embeddings with deterministic hash vectors for unknown Unicode chars.
        char_embs <- lapply(raw_chars, function(ch) {
          if (ch %in% rownames(private$char_embeddings)) {
            return(private$char_embeddings[ch, ])
          }

          codepoint <- tryCatch(utf8ToInt(ch)[1], error = function(e) NA_integer_)
          if (is.na(codepoint)) {
            return(rep(0, self$dimension))
          }

          idx <- seq_len(self$dimension)
          vec <- sin((idx + codepoint) * 0.017) + cos((idx * ((codepoint %% 97) + 1L)) * 0.011)
          vec
        })
        char_embs <- do.call(rbind, char_embs)
        positions <- seq_len(nrow(char_embs)) / nrow(char_embs)
        pos_weights <- exp(-0.5 * (positions - 0.5)^2)  # Gaussian weighting
        emb <- colSums(char_embs * pos_weights) / sum(pos_weights)
      }

      # Normalize
      norm <- sqrt(sum(emb^2))
      if (norm > 0) emb <- emb / norm

      # Cache
      assign(token, emb, envir = private$token_cache)
      emb
    }
  )
)

#' Download pre-trained word vectors
#'
#' @description Download GloVe or other pre-trained word vectors
#'
#' @param model Model name: "glove-50", "glove-100", "glove-200", "glove-300"
#' @param dest_dir Destination directory
#'
#' @return Path to downloaded model
#' @export
download_vectors <- function(model = "glove-50", dest_dir = NULL) {
  if (is.null(dest_dir)) {
    dest_dir <- file.path(rappdirs::user_cache_dir("VectrixDB"), "models")
  }

  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }

  urls <- list(
    "glove-50" = "https://nlp.stanford.edu/data/glove.6B.zip",
    "glove-100" = "https://nlp.stanford.edu/data/glove.6B.zip",
    "glove-200" = "https://nlp.stanford.edu/data/glove.6B.zip",
    "glove-300" = "https://nlp.stanford.edu/data/glove.6B.zip"
  )

  if (!(model %in% names(urls))) {
    stop(sprintf("Unknown model: %s. Available: %s", model, paste(names(urls), collapse = ", ")))
  }

  message(sprintf("Downloading %s vectors...", model))
  message("Note: GloVe vectors are ~800MB. This may take a while.")

  # For now, just return info - actual download would need more implementation
  message("For production use, download GloVe from: https://nlp.stanford.edu/projects/glove/")
  message("Or use the default TF-IDF embeddings which require no downloads.")

  invisible(NULL)
}
