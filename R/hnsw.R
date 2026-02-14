#' VectrixDB HNSW Index
#'
#' @description Hierarchical Navigable Small World graph for fast approximate nearest neighbor search
#'
#' Uses RcppAnnoy for high-performance ANN search.
#' Falls back to brute-force search if RcppAnnoy is not available.
#'
#' @name hnsw
NULL

# Note: DistanceMetric is defined in types.R

# =============================================================================
# Base HNSW Index
# =============================================================================

#' HNSW Index
#'
#' @description High-performance approximate nearest neighbor index
#'
#' @examples
#' \dontrun{
#' # Create index
#' index <- HNSWIndex$new(dimension = 128, metric = "angular")
#'
#' # Add vectors
#' index$add_items(ids = c("a", "b", "c"),
#'                 vectors = matrix(rnorm(384), nrow = 3))
#'
#' # Search
#' results <- index$search(query = rnorm(128), k = 5)
#' }
#'
#' @export
HNSWIndex <- R6::R6Class(
  "HNSWIndex",
  public = list(
    #' @field dimension Vector dimension
    dimension = NULL,
    #' @field metric Distance metric
    metric = "angular",
    #' @field n_trees Number of trees (for Annoy)
    n_trees = 50,
    #' @field search_k Search parameter
    search_k = -1,

    #' @description Create a new HNSWIndex
    #' @param dimension Vector dimension
    #' @param metric Distance metric: "angular", "euclidean", "manhattan", "dot"
    #' @param n_trees Number of trees for index (higher = more accuracy)
    #' @param search_k Search parameter (higher = more accuracy, -1 = auto)
    initialize = function(dimension,
                          metric = "angular",
                          n_trees = 50,
                          search_k = -1) {
      self$dimension <- dimension
      self$metric <- metric
      self$n_trees <- n_trees
      self$search_k <- search_k

      private$id_map <- list()
      private$reverse_map <- list()
      private$vectors <- list()
      private$next_idx <- 0

      private$init_annoy()
    },

    #' @description Add items to the index
    #' @param ids Character vector of IDs
    #' @param vectors Matrix of vectors (rows = items)
    #' @return Self
    add_items = function(ids, vectors) {
      if (is.vector(vectors)) {
        vectors <- matrix(vectors, nrow = 1)
      }

      n <- length(ids)
      if (nrow(vectors) != n) {
        stop("Number of IDs must match number of vectors")
      }
      if (ncol(vectors) != self$dimension) {
        stop(sprintf("Vector dimension mismatch: expected %d, got %d",
                     self$dimension, ncol(vectors)))
      }

      for (i in seq_len(n)) {
        idx <- private$next_idx
        private$id_map[[ids[i]]] <- idx
        private$reverse_map[[as.character(idx)]] <- ids[i]
        private$vectors[[ids[i]]] <- vectors[i, ]

        if (!is.null(private$annoy)) {
          private$annoy$addItem(idx, vectors[i, ])
        }

        private$next_idx <- private$next_idx + 1
      }

      private$is_built <- FALSE
      invisible(self)
    },

    #' @description Build the index (required before searching)
    #' @return Self
    build = function() {
      if (!is.null(private$annoy)) {
        private$annoy$build(self$n_trees)
      }
      private$is_built <- TRUE
      invisible(self)
    },

    #' @description Search for nearest neighbors
    #' @param query Query vector
    #' @param k Number of neighbors
    #' @param include_distances Return distances
    #' @return Data frame with id, distance columns
    search = function(query, k = 10, include_distances = TRUE) {
      if (!private$is_built && length(private$id_map) > 0) {
        self$build()
      }

      if (length(private$id_map) == 0) {
        return(data.frame(id = character(0), distance = numeric(0),
                          stringsAsFactors = FALSE))
      }

      k <- min(k, length(private$id_map))

      if (!is.null(private$annoy)) {
        return(private$search_annoy(query, k, include_distances))
      } else {
        return(private$search_brute_force(query, k, include_distances))
      }
    },

    #' @description Get vector by ID
    #' @param id Item ID
    #' @return Vector or NULL
    get_vector = function(id) {
      private$vectors[[id]]
    },

    #' @description Get all IDs
    #' @return Character vector
    get_ids = function() {
      names(private$id_map)
    },

    #' @description Get item count
    #' @return Integer
    size = function() {
      length(private$id_map)
    },

    #' @description Remove items from index
    #' @param ids IDs to remove
    #' @return Self
    remove_items = function(ids) {
      for (id in ids) {
        if (!is.null(private$id_map[[id]])) {
          idx <- private$id_map[[id]]
          private$id_map[[id]] <- NULL
          private$reverse_map[[as.character(idx)]] <- NULL
          private$vectors[[id]] <- NULL
        }
      }

      # Rebuild index (Annoy doesn't support item removal)
      if (!is.null(private$annoy) && length(private$id_map) > 0) {
        private$rebuild_annoy()
      }

      invisible(self)
    },

    #' @description Clear the index
    #' @return Self
    clear = function() {
      private$id_map <- list()
      private$reverse_map <- list()
      private$vectors <- list()
      private$next_idx <- 0
      private$is_built <- FALSE
      private$init_annoy()
      invisible(self)
    },

    #' @description Save index to file
    #' @param path File path
    save = function(path) {
      data <- list(
        dimension = self$dimension,
        metric = self$metric,
        n_trees = self$n_trees,
        id_map = private$id_map,
        reverse_map = private$reverse_map,
        vectors = private$vectors,
        next_idx = private$next_idx
      )

      saveRDS(data, path)

      # Save Annoy index if available
      if (!is.null(private$annoy)) {
        annoy_path <- paste0(path, ".annoy")
        private$annoy$save(annoy_path)
      }

      invisible(self)
    },

    #' @description Load index from file
    #' @param path File path
    #' @return Self
    load = function(path) {
      data <- readRDS(path)

      self$dimension <- data$dimension
      self$metric <- data$metric
      self$n_trees <- data$n_trees
      private$id_map <- data$id_map
      private$reverse_map <- data$reverse_map
      private$vectors <- data$vectors
      private$next_idx <- data$next_idx

      # Load Annoy index if available
      annoy_path <- paste0(path, ".annoy")
      if (file.exists(annoy_path)) {
        private$init_annoy()
        if (!is.null(private$annoy)) {
          private$annoy$load(annoy_path)
          private$is_built <- TRUE
        }
      }

      invisible(self)
    }
  ),

  private = list(
    annoy = NULL,
    id_map = NULL,
    reverse_map = NULL,
    vectors = NULL,
    next_idx = 0,
    is_built = FALSE,

    init_annoy = function() {
      tryCatch({
        if (requireNamespace("RcppAnnoy", quietly = TRUE)) {
          # Map metric names
          metric_class <- switch(self$metric,
            "angular" = "AnnoyAngular",
            "cosine" = "AnnoyAngular",
            "euclidean" = "AnnoyEuclidean",
            "manhattan" = "AnnoyManhattan",
            "AnnoyAngular"  # default
          )

          private$annoy <- RcppAnnoy::new(metric_class, self$dimension)
        } else {
          message("RcppAnnoy not available. Using brute-force search (slower).")
          private$annoy <- NULL
        }
      }, error = function(e) {
        warning(sprintf("Could not initialize Annoy: %s", e$message))
        private$annoy <- NULL
      })
    },

    rebuild_annoy = function() {
      private$init_annoy()
      if (!is.null(private$annoy)) {
        new_idx <- 0
        new_id_map <- list()
        new_reverse_map <- list()

        for (id in names(private$vectors)) {
          vec <- private$vectors[[id]]
          private$annoy$addItem(new_idx, vec)
          new_id_map[[id]] <- new_idx
          new_reverse_map[[as.character(new_idx)]] <- id
          new_idx <- new_idx + 1
        }

        private$id_map <- new_id_map
        private$reverse_map <- new_reverse_map
        private$next_idx <- new_idx
        private$annoy$build(self$n_trees)
        private$is_built <- TRUE
      }
    },

    search_annoy = function(query, k, include_distances) {
      search_k <- if (self$search_k < 0) k * self$n_trees else self$search_k
      result <- private$annoy$getNNsByVector(query, k, search_k, include_distances)

      if (include_distances) {
        ids <- sapply(result$item, function(idx) {
          private$reverse_map[[as.character(idx)]] %||% NA
        })
        data.frame(
          id = ids,
          distance = result$distance,
          stringsAsFactors = FALSE
        )
      } else {
        ids <- sapply(result, function(idx) {
          private$reverse_map[[as.character(idx)]] %||% NA
        })
        data.frame(
          id = ids,
          distance = rep(NA_real_, length(ids)),
          stringsAsFactors = FALSE
        )
      }
    },

    search_brute_force = function(query, k, include_distances) {
      if (length(private$vectors) == 0) {
        return(data.frame(id = character(0), distance = numeric(0),
                          stringsAsFactors = FALSE))
      }

      # Compute distances
      ids <- names(private$vectors)
      distances <- sapply(private$vectors, function(vec) {
        private$compute_distance(query, vec)
      })

      # Sort and get top k
      order_idx <- order(distances)[1:min(k, length(distances))]

      data.frame(
        id = ids[order_idx],
        distance = distances[order_idx],
        stringsAsFactors = FALSE
      )
    },

    compute_distance = function(a, b) {
      if (self$metric %in% c("angular", "cosine")) {
        # Cosine distance = 1 - cosine_similarity
        dot <- sum(a * b)
        norm_a <- sqrt(sum(a^2))
        norm_b <- sqrt(sum(b^2))
        if (norm_a == 0 || norm_b == 0) return(1)
        1 - dot / (norm_a * norm_b)
      } else if (self$metric == "euclidean") {
        sqrt(sum((a - b)^2))
      } else if (self$metric == "manhattan") {
        sum(abs(a - b))
      } else if (self$metric == "dot") {
        -sum(a * b)  # Negative for "smaller is better"
      } else {
        sqrt(sum((a - b)^2))  # Default to Euclidean
      }
    }
  )
)

# =============================================================================
# Factory Functions
# =============================================================================

#' Create HNSW Index
#'
#' @description Factory function to create HNSW index
#' @param dimension Vector dimension
#' @param metric Distance metric
#' @param n_trees Number of trees
#' @return HNSWIndex object
#' @export
create_hnsw_index <- function(dimension, metric = "angular", n_trees = 50) {
  HNSWIndex$new(
    dimension = dimension,
    metric = metric,
    n_trees = n_trees
  )
}

#' Load HNSW Index
#'
#' @description Load saved index from file
#' @param path File path
#' @return HNSWIndex object
#' @export
load_hnsw_index <- function(path) {
  data <- readRDS(path)
  index <- HNSWIndex$new(
    dimension = data$dimension,
    metric = data$metric,
    n_trees = data$n_trees
  )
  index$load(path)
  index
}

#' Null coalescing for hnsw.R
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a
