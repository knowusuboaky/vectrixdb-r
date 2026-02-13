#' Word Vector Management
#'
#' @description Download, load, and use pre-trained word vectors (GloVe, fastText)
#'
#' @name word_vectors
NULL

# Global cache for word vectors
.wv_cache <- new.env(parent = emptyenv())

#' Download pre-trained word vectors
#'
#' @description Downloads GloVe or fastText word vectors
#'
#' @param model Model to download: "glove-50", "glove-100", "glove-200", "glove-300",
#'              "glove-twitter-25", "glove-twitter-50", "glove-twitter-100", "glove-twitter-200"
#' @param dest_dir Destination directory (default: user cache)
#' @param overwrite Overwrite existing files
#'
#' @return Path to the downloaded vectors file
#' @export
#'
#' @examples
#' \dontrun{
#' # Download 100-dimensional GloVe vectors (~130MB)
#' path <- download_word_vectors("glove-100")
#'
#' # Use with Vectrix
#' db <- Vectrix$new("docs", model = "glove", model_path = path)
#' }
download_word_vectors <- function(model = "glove-100", dest_dir = NULL, overwrite = FALSE) {

  # Model configurations
  models <- list(
    "glove-50" = list(
      url = "https://nlp.stanford.edu/data/glove.6B.zip",
      file = "glove.6B.50d.txt",
      size = "~65MB",
      dim = 50
    ),
    "glove-100" = list(
      url = "https://nlp.stanford.edu/data/glove.6B.zip",
      file = "glove.6B.100d.txt",
      size = "~130MB",
      dim = 100
    ),
    "glove-200" = list(
      url = "https://nlp.stanford.edu/data/glove.6B.zip",
      file = "glove.6B.200d.txt",
      size = "~260MB",
      dim = 200
    ),
    "glove-300" = list(
      url = "https://nlp.stanford.edu/data/glove.6B.zip",
      file = "glove.6B.300d.txt",
      size = "~390MB",
      dim = 300
    ),
    "glove-twitter-25" = list(
      url = "https://nlp.stanford.edu/data/glove.twitter.27B.zip",
      file = "glove.twitter.27B.25d.txt",
      size = "~150MB",
      dim = 25
    ),
    "glove-twitter-50" = list(
      url = "https://nlp.stanford.edu/data/glove.twitter.27B.zip",
      file = "glove.twitter.27B.50d.txt",
      size = "~300MB",
      dim = 50
    ),
    "glove-twitter-100" = list(
      url = "https://nlp.stanford.edu/data/glove.twitter.27B.zip",
      file = "glove.twitter.27B.100d.txt",
      size = "~600MB",
      dim = 100
    ),
    "glove-twitter-200" = list(
      url = "https://nlp.stanford.edu/data/glove.twitter.27B.zip",
      file = "glove.twitter.27B.200d.txt",
      size = "~1.2GB",
      dim = 200
    )
  )

  if (!(model %in% names(models))) {
    stop(sprintf("Unknown model: '%s'. Available: %s",
                 model, paste(names(models), collapse = ", ")))
  }

  config <- models[[model]]

  # Set destination directory
  if (is.null(dest_dir)) {
    dest_dir <- file.path(
      Sys.getenv("VECTRIXDB_CACHE",
                 unset = file.path(Sys.getenv("HOME"), ".vectrixdb")),
      "models"
    )
  }

  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }

  dest_file <- file.path(dest_dir, config$file)

  # Check if already exists
 if (file.exists(dest_file) && !overwrite) {
    message(sprintf("Model already exists: %s", dest_file))
    return(dest_file)
  }

  # Download
  zip_file <- file.path(dest_dir, basename(config$url))

  message(sprintf("Downloading %s (%s)...", model, config$size))
  message(sprintf("URL: %s", config$url))
  message("This may take several minutes...")

  tryCatch({
    download.file(config$url, zip_file, mode = "wb", quiet = FALSE)

    message("Extracting...")
    unzip(zip_file, files = config$file, exdir = dest_dir, overwrite = TRUE)

    # Clean up zip
    unlink(zip_file)

    message(sprintf("Downloaded to: %s", dest_file))
    return(dest_file)

  }, error = function(e) {
    stop(sprintf("Download failed: %s\n\nManual download:\n1. Go to: %s\n2. Extract %s to: %s",
                 e$message, config$url, config$file, dest_dir))
  })
}

#' Load word vectors into memory
#'
#' @description Loads pre-trained word vectors from a file
#'
#' @param path Path to word vectors file (GloVe .txt or word2vec .bin)
#' @param max_words Maximum number of words to load (NULL for all)
#' @param normalize Normalize vectors to unit length
#'
#' @return WordVectors object
#' @export
load_word_vectors <- function(path, max_words = NULL, normalize = TRUE) {

  # Check cache
  cache_key <- paste0(path, "_", max_words %||% "all")
  if (exists(cache_key, envir = .wv_cache)) {
    message("Using cached word vectors")
    return(get(cache_key, envir = .wv_cache))
  }

  if (!file.exists(path)) {
    stop(sprintf("File not found: %s\nRun download_word_vectors() first.", path))
  }

  message(sprintf("Loading word vectors from: %s", path))

  ext <- tolower(tools::file_ext(path))

  if (ext == "bin") {
    # word2vec binary format
    wv <- load_word2vec_binary(path, max_words, normalize)
  } else {
    # GloVe text format
    wv <- load_glove_text(path, max_words, normalize)
  }

  # Cache
  assign(cache_key, wv, envir = .wv_cache)

  message(sprintf("Loaded %d words with %d dimensions", nrow(wv$vectors), ncol(wv$vectors)))
  wv
}

#' Load GloVe text format
#' @keywords internal
load_glove_text <- function(path, max_words = NULL, normalize = TRUE) {

  # Count lines for progress
  message("Reading file...")

  # Read efficiently
  con <- file(path, "r")
  on.exit(close(con))

  # Read first line to get dimension
  first_line <- readLines(con, n = 1)
  parts <- strsplit(first_line, " ")[[1]]
  dim <- length(parts) - 1

  # Reset and read all
  close(con)
  con <- file(path, "r")

  lines <- readLines(con, n = max_words %||% -1, warn = FALSE)
  n <- length(lines)

  message(sprintf("Parsing %d word vectors...", n))

  # Pre-allocate
  words <- character(n)
  vectors <- matrix(0, nrow = n, ncol = dim)

  # Parse lines
  for (i in seq_len(n)) {
    if (i %% 50000 == 0) {
      message(sprintf("  %d / %d (%.1f%%)", i, n, 100 * i / n))
    }

    parts <- strsplit(lines[i], " ", fixed = TRUE)[[1]]
    words[i] <- parts[1]
    vectors[i, ] <- as.numeric(parts[-1])
  }

  # Normalize
  if (normalize) {
    norms <- sqrt(rowSums(vectors^2))
    norms[norms == 0] <- 1
    vectors <- vectors / norms
  }

  rownames(vectors) <- words

  list(
    vectors = vectors,
    words = words,
    dim = dim,
    word_to_idx = stats::setNames(seq_len(n), words)
  )
}

#' Load word2vec binary format
#' @keywords internal
load_word2vec_binary <- function(path, max_words = NULL, normalize = TRUE) {
  model <- read_word2vec_model(path, normalize = normalize)
  vectors <- as.matrix(model)

  list(
    vectors = vectors,
    words = rownames(vectors),
    dim = ncol(vectors),
    word_to_idx = stats::setNames(seq_len(nrow(vectors)), rownames(vectors))
  )
}

#' Sentence Embedder using Word Vectors
#'
#' @description Creates sentence embeddings by averaging word vectors with IDF weighting
#'
#' @export
SentenceEmbedder <- R6::R6Class(
  "SentenceEmbedder",
  public = list(
    #' @field dim Embedding dimension
    dim = NULL,
    #' @field vocab_size Vocabulary size
    vocab_size = NULL,

    #' @description Create a new SentenceEmbedder
    #' @param word_vectors WordVectors object from load_word_vectors()
    #' @param use_idf Use IDF weighting (recommended)
    #' @param smooth_idf Smoothing for IDF
    initialize = function(word_vectors, use_idf = TRUE, smooth_idf = 1) {
      private$wv <- word_vectors
      private$use_idf <- use_idf
      private$smooth_idf <- smooth_idf

      self$dim <- word_vectors$dim
      self$vocab_size <- length(word_vectors$words)

      # Initialize IDF weights (will be computed on first corpus)
      private$idf <- NULL
    },

    #' @description Fit IDF weights on a corpus
    #' @param texts Character vector of texts
    fit = function(texts) {
      if (!private$use_idf) return(invisible(self))

      # Tokenize all texts
      doc_tokens <- lapply(texts, private$tokenize)

      # Count document frequency
      all_words <- unique(unlist(doc_tokens))
      n_docs <- length(texts)

      df <- sapply(all_words, function(w) {
        sum(sapply(doc_tokens, function(tokens) w %in% tokens))
      })

      # Compute IDF with smoothing
      private$idf <- log((n_docs + private$smooth_idf) / (df + private$smooth_idf)) + 1
      names(private$idf) <- all_words

      invisible(self)
    },

    #' @description Embed texts to sentence vectors
    #' @param texts Character vector of texts
    #' @return Matrix of embeddings (rows are sentences)
    embed = function(texts) {
      if (is.character(texts) && length(texts) == 1) {
        texts <- c(texts)
      }

      n <- length(texts)
      result <- matrix(0, nrow = n, ncol = self$dim)

      for (i in seq_len(n)) {
        tokens <- private$tokenize(texts[i])

        if (length(tokens) == 0) next

        # Get word vectors for known tokens
        known_tokens <- tokens[tokens %in% private$wv$words]

        if (length(known_tokens) == 0) next

        # Get vectors
        vecs <- private$wv$vectors[known_tokens, , drop = FALSE]

        # Apply IDF weighting if available
        if (private$use_idf && !is.null(private$idf)) {
          weights <- sapply(known_tokens, function(w) {
            private$idf[w] %||% 1
          })
          weights <- weights / sum(weights)
          result[i, ] <- colSums(vecs * weights)
        } else {
          # Simple average
          result[i, ] <- colMeans(vecs)
        }

        # Normalize
        norm <- sqrt(sum(result[i, ]^2))
        if (norm > 0) {
          result[i, ] <- result[i, ] / norm
        }
      }

      result
    },

    #' @description Get word vector for a single word
    #' @param word Word to look up
    #' @return Numeric vector or NULL if not found
    get_word_vector = function(word) {
      word <- tolower(word)
      if (word %in% private$wv$words) {
        private$wv$vectors[word, ]
      } else {
        NULL
      }
    },

    #' @description Check if word is in vocabulary
    #' @param word Word to check
    #' @return Logical
    has_word = function(word) {
      tolower(word) %in% private$wv$words
    },

    #' @description Find most similar words
    #' @param word Query word
    #' @param n Number of results
    #' @return Data frame with word and similarity
    most_similar = function(word, n = 10) {
      word <- tolower(word)
      if (!(word %in% private$wv$words)) {
        stop(sprintf("Word '%s' not in vocabulary", word))
      }

      query_vec <- private$wv$vectors[word, ]
      sims <- as.vector(private$wv$vectors %*% query_vec)

      top_idx <- order(sims, decreasing = TRUE)[1:(n + 1)]
      top_idx <- top_idx[private$wv$words[top_idx] != word][1:n]

      data.frame(
        word = private$wv$words[top_idx],
        similarity = sims[top_idx],
        stringsAsFactors = FALSE
      )
    }
  ),

  private = list(
    wv = NULL,
    use_idf = TRUE,
    smooth_idf = 1,
    idf = NULL,

    tokenize = function(text) {
      tokens <- tolower(text)
      tokens <- gsub("[^a-z0-9' ]", " ", tokens)
      tokens <- strsplit(tokens, "\\s+")[[1]]
      tokens <- tokens[nchar(tokens) > 1]
      tokens
    }
  )
)

#' Create a sentence embedder with automatic download
#'
#' @description Convenience function to create a SentenceEmbedder with GloVe vectors
#'
#' @param model Model name (default: "glove-100")
#' @param use_idf Use IDF weighting
#'
#' @return SentenceEmbedder object
#' @export
#'
#' @examples
#' \dontrun{
#' # Downloads GloVe if not present
#' embedder <- create_sentence_embedder("glove-100")
#'
#' # Embed texts
#' vectors <- embedder$embed(c("Hello world", "Machine learning is cool"))
#' }
create_sentence_embedder <- function(model = "glove-100", use_idf = TRUE) {

  # Download if needed
  path <- download_word_vectors(model)

  # Load vectors
  wv <- load_word_vectors(path)

  # Create embedder
  SentenceEmbedder$new(wv, use_idf = use_idf)
}

#' Null coalescing for word_vectors.R
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a
