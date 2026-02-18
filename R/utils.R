#' VectrixDB Utility Functions
#'
#' @name utils
#' @keywords internal
NULL

#' Generate deterministic ID from text
#'
#' @param text Character string to hash
#' @return Character string (12 character MD5 hash prefix)
#' @keywords internal
generate_id <- function(text) {

  substr(digest::digest(text, algo = "md5"), 1, 12)
}

#' Normalize vectors for cosine similarity
#'
#' @param vectors Matrix of vectors (rows are vectors)
#' @return Normalized matrix
#' @keywords internal
normalize_vectors <- function(vectors) {
  if (is.vector(vectors)) {
    vectors <- matrix(vectors, nrow = 1)
  }
  norms <- sqrt(rowSums(vectors^2))
  norms[norms == 0] <- 1  # Avoid division by zero

  vectors / norms
}

#' Compute cosine similarity
#'
#' @param query_vector Numeric vector (query)
#' @param doc_vectors Matrix of document vectors
#' @return Numeric vector of similarities
#' @keywords internal
cosine_similarity <- function(query_vector, doc_vectors) {
  if (is.vector(doc_vectors)) {
    doc_vectors <- matrix(doc_vectors, nrow = 1)
  }
  query_norm <- sqrt(sum(query_vector^2))
  doc_norms <- sqrt(rowSums(doc_vectors^2))

  if (query_norm == 0) return(rep(0, nrow(doc_vectors)))

  sims <- (doc_vectors %*% query_vector) / (doc_norms * query_norm)
  as.vector(sims)
}

#' Compute euclidean distance
#'
#' @param query_vector Numeric vector (query)
#' @param doc_vectors Matrix of document vectors
#' @return Numeric vector of distances
#' @keywords internal
euclidean_distance <- function(query_vector, doc_vectors) {
  if (is.vector(doc_vectors)) {
    doc_vectors <- matrix(doc_vectors, nrow = 1)
  }
  sqrt(rowSums((sweep(doc_vectors, 2, query_vector))^2))
}

#' Reciprocal Rank Fusion (RRF)
#'
#' @param rankings List of ranked ID vectors
#' @param k RRF constant (default 60)
#' @param weights Optional weights for each ranking
#' @return Named vector of fused scores
#' @keywords internal
rrf_fusion <- function(rankings, k = 60, weights = NULL) {
  if (is.null(weights)) {
    weights <- rep(1 / length(rankings), length(rankings))
  }

  scores <- list()

  for (i in seq_along(rankings)) {
    ranking <- rankings[[i]]
    weight <- weights[i]

    for (rank in seq_along(ranking)) {
      doc_id <- ranking[rank]
      rrf_score <- weight / (k + rank)

      if (is.null(scores[[doc_id]])) {
        scores[[doc_id]] <- 0
      }
      scores[[doc_id]] <- scores[[doc_id]] + rrf_score
    }
  }

  unlist(scores)
}

#' Maximal Marginal Relevance (MMR) reranking
#'
#' @param query_vector Query embedding
#' @param doc_vectors Matrix of document embeddings
#' @param doc_ids Vector of document IDs
#' @param scores Initial relevance scores
#' @param lambda Diversity parameter (0-1)
#' @param limit Number of results to return
#' @return Data frame with reranked results
#' @keywords internal
mmr_rerank <- function(query_vector, doc_vectors, doc_ids, scores,
                       lambda = 0.7, limit = 10) {
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
        sims <- cosine_similarity(doc_vectors[idx, ], selected_vectors)
        diversity <- max(sims)
      }

      lambda * relevance - (1 - lambda) * diversity
    })

    best_idx <- remaining[which.max(mmr_scores)]
    selected <- c(selected, best_idx)
    selected_vectors <- rbind(selected_vectors, doc_vectors[best_idx, ])
    remaining <- setdiff(remaining, best_idx)
  }

  data.frame(
    id = doc_ids[selected],
    score = scores[selected],
    stringsAsFactors = FALSE
  )
}

#' Check if Python module is available
#'
#' @param module Module name
#' @return Logical
#' @keywords internal
check_python_module <- function(module) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    return(FALSE)
  }

  reticulate::py_module_available(module)
}

#' Get or create VectrixDB data directory
#'
#' @param path Optional custom path
#' @return Path to data directory
#' @keywords internal
#' @noRd
default_data_path <- function() {
  file.path(tempdir(), "vectrixdb_data")
}

get_data_path <- function(path = NULL) {

  if (!is.null(path)) {
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE)
    }
    return(normalizePath(path, mustWork = FALSE))
  }


  # Default path
  default_path <- default_data_path()
  if (!dir.exists(default_path)) {
    dir.create(default_path, recursive = TRUE)
  }
  normalizePath(default_path, mustWork = FALSE)
}

#' Format time duration
#'
#' @param ms Time in milliseconds
#' @return Formatted string
#' @keywords internal
format_time <- function(ms) {

  if (ms < 1000) {
    sprintf("%.1fms", ms)
  } else if (ms < 60000) {
    sprintf("%.2fs", ms / 1000)
  } else {
    sprintf("%.1fm", ms / 60000)
  }
}

#' Normalize language tag to supported values
#'
#' @param language Language label ("en", "english", "ml", "multi", etc.)
#' @param default Default language when input is missing/invalid
#' @return "en" or "ml"
#' @keywords internal
normalize_language_tag <- function(language = NULL, default = "en") {
  default_norm <- tolower(trimws(as.character(default %||% "en")))
  if (!(default_norm %in% c("en", "ml"))) {
    default_norm <- "en"
  }

  if (is.null(language)) {
    return(default_norm)
  }

  lang <- tolower(trimws(as.character(language)))
  if (!nzchar(lang)) {
    return(default_norm)
  }

  if (lang %in% c("en", "english")) {
    return("en")
  }

  if (lang %in% c("ml", "multi", "multilingual")) {
    return("ml")
  }

  # Treat unknown labels as multilingual instead of silently forcing English.
  "ml"
}

#' Language-aware tokenizer used across embedders and keyword search
#'
#' @param text Input text
#' @param language "en" or "ml"
#' @param remove_stopwords Remove English stopwords when language is "en"
#' @return Character vector of tokens
#' @keywords internal
tokenize_text_by_language <- function(text, language = "en", remove_stopwords = FALSE) {
  lang <- normalize_language_tag(language, default = "en")
  value <- tolower(as.character(text %||% ""))
  if (!nzchar(value)) {
    return(character(0))
  }

  # EN keeps legacy ASCII behavior. ML is Unicode-aware (\p{L}/\p{N}).
  if (identical(lang, "en")) {
    value <- gsub("[^a-z0-9 ]", " ", value, perl = TRUE)
  } else {
    value <- gsub("[^\\p{L}\\p{N}]+", " ", value, perl = TRUE)
  }

  tokens <- unlist(strsplit(value, "\\s+", perl = TRUE), use.names = FALSE)
  min_chars <- if (identical(lang, "en")) 2L else 1L
  tokens <- tokens[nchar(tokens, type = "chars") >= min_chars]
  tokens <- tokens[nzchar(tokens)]

  if (isTRUE(remove_stopwords) && identical(lang, "en") && length(tokens) > 0) {
    sw <- tryCatch(
      stopwords::stopwords("en"),
      error = function(e) c(
        "the", "a", "an", "is", "are", "was", "were", "be", "been", "being",
        "have", "has", "had", "do", "does", "did", "will", "would", "could",
        "should", "may", "might", "must", "shall", "can", "of", "to", "in",
        "for", "on", "with", "at", "by", "from", "as", "into", "through"
      )
    )
    tokens <- tokens[!(tokens %in% sw)]
  }

  tokens
}

#' Check whether word2vec package is installed
#'
#' @return Logical
#' @keywords internal
word2vec_available <- function() {
  length(find.package("word2vec", quiet = TRUE)) > 0
}

#' Load word2vec model via optional runtime namespace lookup
#'
#' @param path Path to .bin model
#' @param normalize Whether to normalize embeddings (forwarded when supported)
#' @return word2vec model object
#' @keywords internal
read_word2vec_model <- function(path, normalize = TRUE) {
  if (!word2vec_available()) {
    stop("word2vec package required for .bin files. Install with: install.packages('word2vec')")
  }

  read_fun <- get("read.word2vec", envir = asNamespace("word2vec"), inherits = FALSE)
  read_fun(path, normalize = normalize)
}
