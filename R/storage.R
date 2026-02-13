#' VectrixDB Storage Classes
#'
#' @description Storage backends for VectrixDB
#'
#' @name storage
NULL

#' In-Memory Storage
#'
#' @description Fast in-memory storage backend
#'
#' @keywords internal
InMemoryStorage <- R6::R6Class(
  "InMemoryStorage",
  public = list(
    #' @field data Storage data
    data = NULL,

    #' @description Create new in-memory storage
    initialize = function() {
      self$data <- new.env(hash = TRUE)
    },

    #' @description Store a value
    #' @param key Storage key
    #' @param value Value to store
    set = function(key, value) {
      self$data[[key]] <- value
    },

    #' @description Retrieve a value
    #' @param key Storage key
    #' @return Stored value or NULL
    get = function(key) {
      self$data[[key]]
    },

    #' @description Delete a value
    #' @param key Storage key
    delete = function(key) {
      if (exists(key, envir = self$data)) {
        rm(list = key, envir = self$data)
      }
    },

    #' @description Check if key exists
    #' @param key Storage key
    #' @return Logical
    exists = function(key) {
      exists(key, envir = self$data)
    },

    #' @description List all keys
    #' @return Character vector of keys
    keys = function() {
      ls(self$data)
    },

    #' @description Clear all data
    clear = function() {
      rm(list = ls(self$data), envir = self$data)
    },

    #' @description Get count of stored items
    #' @return Integer count
    count = function() {
      length(ls(self$data))
    }
  )
)

#' SQLite Storage
#'
#' @description Persistent SQLite storage backend
#'
#' @keywords internal
SQLiteStorage <- R6::R6Class(
  "SQLiteStorage",
  public = list(
    #' @field db_path Database file path
    db_path = NULL,
    #' @field conn Database connection
    conn = NULL,

    #' @description Create new SQLite storage
    #' @param path Database file path
    initialize = function(path) {
      self$db_path <- path

      if (!requireNamespace("RSQLite", quietly = TRUE)) {
        stop("RSQLite package required. Install with: install.packages('RSQLite')")
      }

      if (!requireNamespace("DBI", quietly = TRUE)) {
        stop("DBI package required. Install with: install.packages('DBI')")
      }

      self$conn <- DBI::dbConnect(RSQLite::SQLite(), path)
      private$init_tables()
    },

    #' @description Store a value
    #' @param key Storage key
    #' @param value Value to store
    set = function(key, value) {
      value_json <- jsonlite::toJSON(value, auto_unbox = TRUE)
      DBI::dbExecute(
        self$conn,
        "INSERT OR REPLACE INTO storage (key, value) VALUES (?, ?)",
        params = list(key, as.character(value_json))
      )
    },

    #' @description Retrieve a value
    #' @param key Storage key
    #' @return Stored value or NULL
    get = function(key) {
      result <- DBI::dbGetQuery(
        self$conn,
        "SELECT value FROM storage WHERE key = ?",
        params = list(key)
      )
      if (nrow(result) == 0) return(NULL)
      jsonlite::fromJSON(result$value[1])
    },

    #' @description Delete a value
    #' @param key Storage key
    delete = function(key) {
      DBI::dbExecute(
        self$conn,
        "DELETE FROM storage WHERE key = ?",
        params = list(key)
      )
    },

    #' @description Check if key exists
    #' @param key Storage key
    #' @return Logical
    exists = function(key) {
      result <- DBI::dbGetQuery(
        self$conn,
        "SELECT 1 FROM storage WHERE key = ? LIMIT 1",
        params = list(key)
      )
      nrow(result) > 0
    },

    #' @description List all keys
    #' @return Character vector of keys
    keys = function() {
      result <- DBI::dbGetQuery(self$conn, "SELECT key FROM storage")
      result$key
    },

    #' @description Clear all data
    clear = function() {
      DBI::dbExecute(self$conn, "DELETE FROM storage")
    },

    #' @description Get count of stored items
    #' @return Integer count
    count = function() {
      result <- DBI::dbGetQuery(self$conn, "SELECT COUNT(*) as n FROM storage")
      result$n[1]
    },

    #' @description Close database connection
    close = function() {
      if (!is.null(self$conn)) {
        DBI::dbDisconnect(self$conn)
        self$conn <- NULL
      }
    }
  ),
  private = list(
    finalize = function() {
      self$close()
    },
    init_tables = function() {
      DBI::dbExecute(self$conn, "
        CREATE TABLE IF NOT EXISTS storage (
          key TEXT PRIMARY KEY,
          value TEXT
        )
      ")
    }
  )
)

#' Collection Class
#'
#' @description Vector collection with indexing and search
#'
#' @export
Collection <- R6::R6Class(
  "Collection",
  public = list(
    #' @field name Collection name
    name = NULL,
    #' @field dimension Vector dimension
    dimension = NULL,
    #' @field metric Distance metric
    metric = NULL,
    #' @field language Language setting ("en" or "ml")
    language = "en",

    #' @description Create a new Collection
    #' @param name Collection name
    #' @param dimension Vector dimension
    #' @param metric Distance metric
    #' @param storage Storage backend
    #' @param language Language behavior ("en" = ASCII-focused, "ml" = Unicode-aware)
    initialize = function(name, dimension, metric = "cosine", storage = NULL, language = "en") {
      self$name <- name
      self$dimension <- dimension
      self$metric <- metric
      self$language <- normalize_language_tag(language, default = "en")

      private$storage <- storage %||% InMemoryStorage$new()
      private$vectors <- matrix(nrow = 0, ncol = dimension)
      private$ids <- character(0)
      private$metadata <- list()
      private$texts <- list()
    },

    #' @description Add documents to collection
    #' @param ids Document IDs
    #' @param vectors Matrix of vectors
    #' @param metadata List of metadata
    #' @param texts Character vector of texts
    add = function(ids, vectors, metadata = NULL, texts = NULL) {
      if (is.vector(vectors) && !is.list(vectors)) {
        vectors <- matrix(vectors, nrow = 1)
      }

      n <- length(ids)

      if (is.null(metadata)) {
        metadata <- replicate(n, list(), simplify = FALSE)
      }

      if (is.null(texts)) {
        texts <- rep("", n)
      }

      # Handle dimension changes (e.g., TF-IDF vocabulary grows)
      new_dim <- ncol(vectors)
      if (nrow(private$vectors) == 0) {
        # First add - just use the vectors directly
        private$vectors <- vectors
        self$dimension <- new_dim
      } else if (new_dim != self$dimension) {
        # Dimension changed - reinitialize (this shouldn't happen often)
        warning("Vector dimension changed from ", self$dimension, " to ", new_dim)
        self$dimension <- new_dim
        private$vectors <- vectors
        # Clear old data as dimensions are incompatible
        private$ids <- character(0)
        private$metadata <- list()
        private$texts <- list()
      } else {
        # Normal case - same dimension
        private$vectors <- rbind(private$vectors, vectors)
      }
      private$ids <- c(private$ids, ids)

      for (i in seq_len(n)) {
        private$metadata[[ids[i]]] <- metadata[[i]]
        private$texts[[ids[i]]] <- texts[i]
      }

      # Rebuild index if using ANN
      if (!is.null(private$ann_index)) {
        private$build_index()
      }

      invisible(self)
    },

    #' @description Search collection
    #' @param query Query vector
    #' @param limit Number of results
    #' @param filter Metadata filter
    #' @param include_vectors Include vectors in results
    #' @return Results object
    search = function(query, limit = 10, filter = NULL, include_vectors = FALSE) {
      if (length(private$ids) == 0) {
        return(Results$new(items = list(), query = "", mode = "dense", time_ms = 0))
      }

      start_time <- Sys.time()

      # Compute similarities
      if (self$metric == "cosine") {
        scores <- cosine_similarity(query, private$vectors)
      } else if (self$metric == "euclidean") {
        scores <- -euclidean_distance(query, private$vectors)  # Negate for ranking
      } else {
        scores <- as.vector(private$vectors %*% query)  # Dot product
      }

      # Apply filter if provided
      if (!is.null(filter)) {
        mask <- private$apply_filter(filter)
        scores[!mask] <- -Inf
      }

      # Get top results
      top_idx <- order(scores, decreasing = TRUE)[1:min(limit, length(scores))]
      top_idx <- top_idx[scores[top_idx] > -Inf]

      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs")) * 1000

      items <- lapply(top_idx, function(idx) {
        Result$new(
          id = private$ids[idx],
          text = private$texts[[private$ids[idx]]] %||% "",
          score = scores[idx],
          metadata = private$metadata[[private$ids[idx]]] %||% list()
        )
      })

      Results$new(items = items, query = "", mode = "dense", time_ms = elapsed)
    },

    #' @description Keyword search
    #' @param query_text Query text
    #' @param limit Number of results
    #' @param filter Metadata filter
    #' @return Results object
    keyword_search = function(query_text, limit = 10, filter = NULL) {
      if (length(private$ids) == 0) {
        return(Results$new(items = list(), query = query_text, mode = "sparse", time_ms = 0))
      }

      start_time <- Sys.time()

      # Simple keyword matching
      query_tokens <- private$tokenize(query_text)

      scores <- sapply(private$ids, function(id) {
        text <- private$texts[[id]] %||% ""
        doc_tokens <- private$tokenize(text)

        if (length(query_tokens) == 0 || length(doc_tokens) == 0) {
          return(0)
        }

        # BM25-like scoring
        intersection <- length(intersect(query_tokens, doc_tokens))
        intersection / (length(query_tokens) + 0.5)
      })

      # Apply filter if provided
      if (!is.null(filter)) {
        mask <- private$apply_filter(filter)
        scores[!mask] <- -Inf
      }

      # Get top results
      top_idx <- order(scores, decreasing = TRUE)[1:min(limit, length(scores))]
      top_idx <- top_idx[scores[top_idx] > 0]

      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs")) * 1000

      items <- lapply(top_idx, function(idx) {
        Result$new(
          id = private$ids[idx],
          text = private$texts[[private$ids[idx]]] %||% "",
          score = scores[idx],
          metadata = private$metadata[[private$ids[idx]]] %||% list()
        )
      })

      Results$new(items = items, query = query_text, mode = "sparse", time_ms = elapsed)
    },

    #' @description Hybrid search (dense + sparse)
    #' @param query Query vector
    #' @param query_text Query text
    #' @param limit Number of results
    #' @param vector_weight Weight for vector search
    #' @param text_weight Weight for text search
    #' @param filter Metadata filter
    #' @param include_vectors Include vectors in results
    #' @param rrf_k RRF constant
    #' @param prefetch_multiplier Prefetch multiplier
    #' @return Results object
    hybrid_search = function(query, query_text, limit = 10,
                              vector_weight = 0.5, text_weight = 0.5,
                              filter = NULL, include_vectors = FALSE,
                              rrf_k = 60, prefetch_multiplier = 10) {
      start_time <- Sys.time()

      prefetch_limit <- min(limit * prefetch_multiplier, length(private$ids))

      # Dense search
      dense_results <- self$search(query, limit = prefetch_limit, filter = filter)

      # Sparse search
      sparse_results <- self$keyword_search(query_text, limit = prefetch_limit, filter = filter)

      # RRF Fusion
      rankings <- list(
        dense_results$ids(),
        sparse_results$ids()
      )
      weights <- c(vector_weight, text_weight)

      fused_scores <- rrf_fusion(rankings, k = rrf_k, weights = weights)

      # Sort by fused score
      sorted_ids <- names(sort(fused_scores, decreasing = TRUE))[1:min(limit, length(fused_scores))]

      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs")) * 1000

      items <- lapply(sorted_ids, function(id) {
        Result$new(
          id = id,
          text = private$texts[[id]] %||% "",
          score = fused_scores[id],
          metadata = private$metadata[[id]] %||% list()
        )
      })

      Results$new(items = items, query = query_text, mode = "hybrid", time_ms = elapsed)
    },

    #' @description Get documents by ID
    #' @param ids Document IDs
    #' @return List of results
    get = function(ids) {
      lapply(ids, function(id) {
        idx <- match(id, private$ids)
        if (is.na(idx)) return(NULL)

        list(
          id = id,
          vector = private$vectors[idx, ],
          metadata = private$metadata[[id]] %||% list(),
          text = private$texts[[id]] %||% ""
        )
      })
    },

    #' @description Delete documents by ID
    #' @param ids Document IDs to delete
    delete = function(ids) {
      mask <- !(private$ids %in% ids)
      private$vectors <- private$vectors[mask, , drop = FALSE]
      private$ids <- private$ids[mask]

      for (id in ids) {
        private$metadata[[id]] <- NULL
        private$texts[[id]] <- NULL
      }

      invisible(self)
    },

    #' @description Get document count
    #' @return Integer count
    count = function() {
      length(private$ids)
    },

    #' @description Clear collection
    clear = function() {
      private$vectors <- matrix(nrow = 0, ncol = self$dimension)
      private$ids <- character(0)
      private$metadata <- list()
      private$texts <- list()
      invisible(self)
    }
  ),
  private = list(
    storage = NULL,
    vectors = NULL,
    ids = NULL,
    metadata = NULL,
    texts = NULL,
    ann_index = NULL,
    use_ann = FALSE,
    ann_trees = 50,

    tokenize = function(text) {
      tokenize_text_by_language(text, language = self$language, remove_stopwords = FALSE)
    },

    apply_filter = function(filter) {
      if (is.null(filter)) return(rep(TRUE, length(private$ids)))

      conditions <- if (inherits(filter, "Filter")) {
        filter$to_list()
      } else if (is.list(filter)) {
        filter
      } else {
        return(rep(TRUE, length(private$ids)))
      }

      sapply(private$ids, function(id) {
        meta <- private$metadata[[id]] %||% list()

        all(sapply(names(conditions), function(field) {
          cond <- conditions[[field]]

          if (is.list(cond) && !is.null(cond$op)) {
            value <- meta[[field]]
            op <- cond$op
            target <- cond$value

            if (is.null(value)) return(FALSE)

            switch(op,
              "eq" = value == target,
              "ne" = value != target,
              "gt" = value > target,
              "lt" = value < target,
              "gte" = value >= target,
              "lte" = value <= target,
              "in" = value %in% target,
              TRUE
            )
          } else {
            # Simple equality
            identical(meta[[field]], cond)
          }
        }))
      })
    },

    build_index = function() {
      # Build ANN index using RcppAnnoy for fast approximate search
      if (!requireNamespace("RcppAnnoy", quietly = TRUE)) {
        private$use_ann <- FALSE
        return(invisible(NULL))
      }

      if (length(private$ids) < 100) {
        # Not worth building index for small collections
        private$use_ann <- FALSE
        return(invisible(NULL))
      }

      tryCatch({
        # Determine metric type for Annoy
        metric_type <- switch(self$metric,
          "cosine" = "angular",
          "euclidean" = "euclidean",
          "dot" = "angular",  # Annoy doesn't have dot, use angular
          "angular"
        )

        # Create new Annoy index
        private$ann_index <- RcppAnnoy::new(RcppAnnoy::AnnoyAngular, self$dimension)

        # Add all vectors
        for (i in seq_len(nrow(private$vectors))) {
          private$ann_index$addItem(i - 1, private$vectors[i, ])
        }

        # Build index with specified number of trees
        private$ann_index$build(private$ann_trees)
        private$use_ann <- TRUE

      }, error = function(e) {
        warning(sprintf("Failed to build ANN index: %s", e$message))
        private$use_ann <- FALSE
      })

      invisible(NULL)
    },

    ann_search = function(query, limit) {
      # Use ANN index for fast approximate search
      if (!private$use_ann || is.null(private$ann_index)) {
        return(NULL)
      }

      tryCatch({
        # Get nearest neighbors (returns 0-indexed)
        result <- private$ann_index$getNNsByVector(query, limit, include_distances = TRUE)

        list(
          indices = result$item + 1,  # Convert to 1-indexed
          distances = result$distance
        )
      }, error = function(e) {
        NULL
      })
    }
  )
)

#' Null coalescing operator
#' @param a First value
#' @param b Default value
#' @return a if not NULL, else b
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a
