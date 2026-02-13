#' VectrixDB Types and Classes
#'
#' Core data types for VectrixDB operations
#'
#' @name types
#' @keywords internal
NULL

#' Search Mode Enumeration
#'
#' @description Available search modes for VectrixDB
#' @export
SearchMode <- list(

  DENSE = "dense",
  SPARSE = "sparse",

  HYBRID = "hybrid",
  ULTIMATE = "ultimate",
  NEURAL = "neural"
)

#' Distance Metric Enumeration
#'
#' @description Available distance metrics for vector comparison
#' @export
DistanceMetric <- list(
  COSINE = "cosine",
  EUCLIDEAN = "euclidean",
  DOT = "dot",
  MANHATTAN = "manhattan"
)

#' Single Search Result
#'
#' @description Represents a single search result with id, text, score, and metadata
#'
#' @export
Result <- R6::R6Class(

  "Result",
  public = list(
    #' @field id Document ID
    id = NULL,
    #' @field text Document text
    text = NULL,
    #' @field score Relevance score
    score = NULL,
    #' @field metadata Document metadata
    metadata = NULL,

    #' @description Create a new Result object
    #' @param id Document ID
    #' @param text Document text
    #' @param score Relevance score
    #' @param metadata Optional metadata list
    initialize = function(id, text, score, metadata = list()) {
      self$id <- id
      self$text <- text
      self$score <- score
      self$metadata <- metadata
    },

    #' @description Print result summary
    print = function() {
      preview <- if (nchar(self$text) > 50) {
        paste0(substr(self$text, 1, 50), "...")
      } else {
        self$text
      }
      cat(sprintf("Result(score=%.4f, text='%s')\n", self$score, preview))
      invisible(self)
    }
  )
)

#' Search Results Collection
#'
#' @description Collection of search results with convenient accessors
#'
#' @export
Results <- R6::R6Class(
  "Results",
  public = list(
    #' @field items List of Result objects
    items = NULL,
    #' @field query Search query
    query = NULL,
    #' @field mode Search mode
    mode = NULL,
    #' @field time_ms Execution time in ms
    time_ms = NULL,

    #' @description Create a new Results object
    #' @param items List of Result objects
    #' @param query Search query string
    #' @param mode Search mode used
    #' @param time_ms Execution time in milliseconds
    initialize = function(items = list(), query = "", mode = "hybrid", time_ms = 0) {
      self$items <- items
      self$query <- query
      self$mode <- mode
      self$time_ms <- time_ms
    },

    #' @description Get number of results
    length = function() {
      length(self$items)
    },

    #' @description Get all result texts
    #' @return Character vector of texts
    texts = function() {
      sapply(self$items, function(r) r$text)
    },

    #' @description Get all result IDs
    #' @return Character vector of IDs
    ids = function() {
      sapply(self$items, function(r) r$id)
    },

    #' @description Get all scores
    #' @return Numeric vector of scores
    scores = function() {
      sapply(self$items, function(r) r$score)
    },

    #' @description Get top result
    #' @return Result object or NULL if empty
    top = function() {
      if (length(self$items) > 0) self$items[[1]] else NULL
    },

    #' @description Get result by index
    #' @param i Index
    #' @return Result object
    get = function(i) {
      self$items[[i]]
    },

    #' @description Iterate over results
    #' @param fn Function to apply to each result
    foreach = function(fn) {
      lapply(self$items, fn)
    },

    #' @description Print results summary
    print = function() {
      query_preview <- if (nchar(self$query) > 30) {
        paste0(substr(self$query, 1, 30), "...")
      } else {
        self$query
      }
      cat(sprintf("Results(%d results for '%s' in %.1fms)\n",
                  length(self$items), query_preview, self$time_ms))
      invisible(self)
    }
  )
)

#' Filter Class for Metadata Filtering
#'
#' @description Build metadata filters for search queries
#'
#' @export
Filter <- R6::R6Class(
  "Filter",
  public = list(
    #' @field conditions List of filter conditions
    conditions = NULL,

    #' @description Create a new Filter
    #' @param ... Named filter conditions
    initialize = function(...) {
      self$conditions <- list(...)
    },

    #' @description Add equality condition
    #' @param field Field name

    #' @param value Value to match
    #' @return Self for chaining
    eq = function(field, value) {
      self$conditions[[field]] <- list(op = "eq", value = value)
      invisible(self)
    },

    #' @description Add not-equal condition
    #' @param field Field name
    #' @param value Value to exclude
    #' @return Self for chaining
    ne = function(field, value) {
      self$conditions[[field]] <- list(op = "ne", value = value)
      invisible(self)
    },

    #' @description Add greater-than condition
    #' @param field Field name
    #' @param value Threshold value
    #' @return Self for chaining
    gt = function(field, value) {
      self$conditions[[field]] <- list(op = "gt", value = value)
      invisible(self)
    },

    #' @description Add less-than condition
    #' @param field Field name
    #' @param value Threshold value
    #' @return Self for chaining
    lt = function(field, value) {
      self$conditions[[field]] <- list(op = "lt", value = value)
      invisible(self)
    },

    #' @description Add in-list condition
    #' @param field Field name
    #' @param values Vector of values
    #' @return Self for chaining
    in_list = function(field, values) {
      self$conditions[[field]] <- list(op = "in", value = values)
      invisible(self)
    },

    #' @description Convert to list for API
    #' @return List representation
    to_list = function() {
      self$conditions
    }
  )
)
