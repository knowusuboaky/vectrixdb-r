#' VectrixDB Cache Layer
#'
#' @description High-performance caching for low latency
#'
#' Supports multiple cache backends:
#' - InMemory LRU: Ultra-fast, limited by RAM
#' - File-based: Persistent cache using RDS files
#'
#' @name cache
NULL

# =============================================================================
# Cache Configuration
# =============================================================================

#' Cache Backend Types
#'
#' @description Available cache backends
#' @export
CacheBackend <- list(
  NONE = "none",
  MEMORY = "memory",
  FILE = "file"
)

#' Cache Configuration
#'
#' @description Configuration for cache layer
#' @export
CacheConfig <- R6::R6Class(
  "CacheConfig",
  public = list(
    #' @field backend Cache backend type
    backend = "memory",
    #' @field memory_max_size Max items in memory
    memory_max_size = 10000,
    #' @field memory_ttl_seconds Default TTL in seconds
    memory_ttl_seconds = 3600,
    #' @field file_cache_dir Directory for file cache
    file_cache_dir = NULL,
    #' @field file_ttl_seconds File cache TTL
    file_ttl_seconds = 86400,
    #' @field prefix Cache key prefix
    prefix = "vectrix:",
    #' @field compression Use compression
    compression = TRUE,

    #' @description Create a new CacheConfig
    #' @param backend Backend type
    #' @param memory_max_size Max memory items
    #' @param memory_ttl_seconds Memory TTL
    #' @param file_cache_dir File cache directory
    #' @param file_ttl_seconds File TTL
    #' @param prefix Key prefix
    #' @param compression Use compression
    initialize = function(backend = "memory",
                          memory_max_size = 10000,
                          memory_ttl_seconds = 3600,
                          file_cache_dir = NULL,
                          file_ttl_seconds = 86400,
                          prefix = "vectrix:",
                          compression = TRUE) {
      self$backend <- backend
      self$memory_max_size <- memory_max_size
      self$memory_ttl_seconds <- memory_ttl_seconds
      self$file_cache_dir <- file_cache_dir %||% file.path(tempdir(), "vectrix_cache")
      self$file_ttl_seconds <- file_ttl_seconds
      self$prefix <- prefix
      self$compression <- compression
    }
  )
)

#' Create Cache Config from Environment
#'
#' @description Create config from environment variables
#' @return CacheConfig object
#' @export
cache_config_from_env <- function() {
  CacheConfig$new(
    backend = Sys.getenv("VECTRIX_CACHE_BACKEND", "memory"),
    memory_max_size = as.integer(Sys.getenv("VECTRIX_CACHE_SIZE", "10000")),
    file_cache_dir = Sys.getenv("VECTRIX_CACHE_DIR", "")
  )
}

# =============================================================================
# Cache Statistics
# =============================================================================

#' Cache Statistics
#'
#' @description Cache statistics for monitoring
#' @export
CacheStats <- R6::R6Class(
  "CacheStats",
  public = list(
    #' @field hits Cache hits
    hits = 0,
    #' @field misses Cache misses
    misses = 0,
    #' @field sets Cache sets
    sets = 0,
    #' @field deletes Cache deletes
    deletes = 0,
    #' @field evictions Cache evictions
    evictions = 0,

    #' @description Record a cache hit
    record_hit = function() {
      self$hits <- self$hits + 1
    },

    #' @description Record a cache miss
    record_miss = function() {
      self$misses <- self$misses + 1
    },

    #' @description Record a cache set
    record_set = function() {
      self$sets <- self$sets + 1
    },

    #' @description Record a cache delete
    record_delete = function() {
      self$deletes <- self$deletes + 1
    },

    #' @description Record a cache eviction
    record_eviction = function() {
      self$evictions <- self$evictions + 1
    },

    #' @description Get hit rate
    #' @return Numeric hit rate
    hit_rate = function() {
      total <- self$hits + self$misses
      if (total > 0) self$hits / total else 0
    },

    #' @description Convert to list
    #' @return List representation
    to_list = function() {
      list(
        hits = self$hits,
        misses = self$misses,
        sets = self$sets,
        deletes = self$deletes,
        evictions = self$evictions,
        hit_rate = sprintf("%.2f%%", self$hit_rate() * 100)
      )
    },

    #' @description Reset statistics
    reset = function() {
      self$hits <- 0
      self$misses <- 0
      self$sets <- 0
      self$deletes <- 0
      self$evictions <- 0
    }
  )
)

# =============================================================================
# Cache Entry
# =============================================================================

#' Cache Entry
#'
#' @description A cached entry with metadata
#' @export
CacheEntry <- R6::R6Class(
  "CacheEntry",
  public = list(
    #' @field value Cached value
    value = NULL,
    #' @field created_at Creation timestamp
    created_at = NULL,
    #' @field ttl Time to live in seconds
    ttl = NULL,
    #' @field hits Number of hits
    hits = 0,

    #' @description Create a new CacheEntry
    #' @param value The value to cache
    #' @param ttl Time to live in seconds
    initialize = function(value, ttl) {
      self$value <- value
      self$created_at <- Sys.time()
      self$ttl <- ttl
      self$hits <- 0
    },

    #' @description Check if entry is expired
    #' @return Logical
    is_expired = function() {
      as.numeric(difftime(Sys.time(), self$created_at, units = "secs")) > self$ttl
    }
  )
)

# =============================================================================
# Base Cache Class
# =============================================================================

#' Base Cache
#'
#' @description Abstract base class for cache backends
#' @export
BaseCache <- R6::R6Class(
  "BaseCache",
  public = list(
    #' @field config Cache configuration
    config = NULL,
    #' @field stats Cache statistics
    stats = NULL,

    #' @description Create a new cache
    #' @param config CacheConfig object
    initialize = function(config = NULL) {
      self$config <- config %||% CacheConfig$new()
      self$stats <- CacheStats$new()
    },

    #' @description Get a value from cache
    #' @param key Cache key
    #' @return Cached value or NULL
    get = function(key) {
      stop("Must be implemented by subclass")
    },

    #' @description Set a value in cache
    #' @param key Cache key
    #' @param value Value to cache
    #' @param ttl Time to live (optional)
    set = function(key, value, ttl = NULL) {
      stop("Must be implemented by subclass")
    },

    #' @description Delete a key from cache
    #' @param key Cache key
    #' @return Logical success
    delete = function(key) {
      stop("Must be implemented by subclass")
    },

    #' @description Check if key exists
    #' @param key Cache key
    #' @return Logical
    exists = function(key) {
      stop("Must be implemented by subclass")
    },

    #' @description Clear all cache entries
    clear = function() {
      stop("Must be implemented by subclass")
    },

    #' @description Get cache size
    #' @return Integer count
    size = function() {
      stop("Must be implemented by subclass")
    },

    #' @description Get multiple values
    #' @param keys Character vector of keys
    #' @return Named list of values
    get_many = function(keys) {
      result <- list()
      for (k in keys) {
        val <- self$get(k)
        if (!is.null(val)) {
          result[[k]] <- val
        }
      }
      result
    },

    #' @description Set multiple values
    #' @param items Named list of values
    #' @param ttl Time to live
    set_many = function(items, ttl = NULL) {
      for (key in names(items)) {
        self$set(key, items[[key]], ttl)
      }
    },

    #' @description Delete multiple keys
    #' @param keys Character vector of keys
    #' @return Integer count of deleted keys
    delete_many = function(keys) {
      count <- 0
      for (key in keys) {
        if (self$delete(key)) {
          count <- count + 1
        }
      }
      count
    },

    #' @description Make a prefixed key
    #' @param key Raw key
    #' @return Prefixed key
    make_key = function(key) {
      paste0(self$config$prefix, key)
    }
  )
)

# =============================================================================
# No-Op Cache
# =============================================================================

#' No-Op Cache
#'
#' @description Disabled cache (no caching)
#' @export
NoCache <- R6::R6Class(
  "NoCache",
  inherit = BaseCache,
  public = list(
    #' @description Get value from cache (always returns NULL)
    #' @param key Cache key
    #' @return NULL
    get = function(key) NULL,

    #' @description Set cache value (no-op)
    #' @param key Cache key
    #' @param value Value to cache
    #' @param ttl Time-to-live in seconds (ignored)
    #' @return Invisibly returns NULL
    set = function(key, value, ttl = NULL) invisible(NULL),

    #' @description Delete key from cache (always FALSE)
    #' @param key Cache key
    #' @return FALSE
    delete = function(key) FALSE,

    #' @description Check if key exists (always FALSE)
    #' @param key Cache key
    #' @return FALSE
    exists = function(key) FALSE,

    #' @description Clear cache (no-op)
    #' @return Invisibly returns NULL
    clear = function() invisible(NULL),

    #' @description Get cache size (always 0)
    #' @return Integer zero
    size = function() 0L
  )
)

# =============================================================================
# Memory Cache (LRU)
# =============================================================================

#' Memory Cache
#'
#' @description In-memory LRU cache with TTL support
#'
#' Ultra-low latency, limited by available RAM.
#' Best for hot data, session data, frequently accessed vectors.
#'
#' @export
MemoryCache <- R6::R6Class(
  "MemoryCache",
  inherit = BaseCache,
  public = list(
    #' @description Create a new MemoryCache
    #' @param config CacheConfig object
    initialize = function(config = NULL) {
      super$initialize(config)
      private$cache <- new.env(hash = TRUE, parent = emptyenv())
      private$access_order <- character(0)
    },

    #' @description Get value from cache
    #' @param key Cache key
    #' @return Value or NULL
    get = function(key) {
      if (!base::exists(key, envir = private$cache)) {
        self$stats$record_miss()
        return(NULL)
      }

      entry <- base::get(key, envir = private$cache)

      if (entry$is_expired()) {
        rm(list = key, envir = private$cache)
        private$access_order <- setdiff(private$access_order, key)
        self$stats$record_miss()
        return(NULL)
      }

      # Move to end (LRU)
      private$access_order <- c(setdiff(private$access_order, key), key)
      entry$hits <- entry$hits + 1
      self$stats$record_hit()
      entry$value
    },

    #' @description Set value in cache
    #' @param key Cache key
    #' @param value Value to cache
    #' @param ttl Time to live
    set = function(key, value, ttl = NULL) {
      ttl <- ttl %||% self$config$memory_ttl_seconds

      # Evict if at capacity
      while (length(private$access_order) >= self$config$memory_max_size) {
        oldest_key <- private$access_order[1]
        rm(list = oldest_key, envir = private$cache)
        private$access_order <- private$access_order[-1]
        self$stats$record_eviction()
      }

      assign(key, CacheEntry$new(value, ttl), envir = private$cache)
      private$access_order <- c(setdiff(private$access_order, key), key)
      self$stats$record_set()
      invisible(NULL)
    },

    #' @description Delete key from cache
    #' @param key Cache key
    #' @return Logical success
    delete = function(key) {
      if (base::exists(key, envir = private$cache)) {
        rm(list = key, envir = private$cache)
        private$access_order <- setdiff(private$access_order, key)
        self$stats$record_delete()
        return(TRUE)
      }
      FALSE
    },

    #' @description Check if key exists
    #' @param key Cache key
    #' @return Logical
    exists = function(key) {
      if (!base::exists(key, envir = private$cache)) {
        return(FALSE)
      }
      entry <- base::get(key, envir = private$cache)
      if (entry$is_expired()) {
        rm(list = key, envir = private$cache)
        private$access_order <- setdiff(private$access_order, key)
        return(FALSE)
      }
      TRUE
    },

    #' @description Clear cache
    clear = function() {
      rm(list = ls(envir = private$cache), envir = private$cache)
      private$access_order <- character(0)
      invisible(NULL)
    },

    #' @description Get cache size
    #' @return Integer
    size = function() {
      length(private$access_order)
    },

    #' @description Cleanup expired entries
    #' @return Integer count removed
    cleanup_expired = function() {
      expired_keys <- c()
      for (key in ls(envir = private$cache)) {
        entry <- base::get(key, envir = private$cache)
        if (entry$is_expired()) {
          expired_keys <- c(expired_keys, key)
        }
      }
      for (key in expired_keys) {
        rm(list = key, envir = private$cache)
      }
      private$access_order <- setdiff(private$access_order, expired_keys)
      length(expired_keys)
    }
  ),

  private = list(
    cache = NULL,
    access_order = NULL
  )
)

# =============================================================================
# File Cache
# =============================================================================

#' File Cache
#'
#' @description File-based persistent cache using RDS files
#' @export
FileCache <- R6::R6Class(
  "FileCache",
  inherit = BaseCache,
  public = list(
    #' @description Create a new FileCache
    #' @param config CacheConfig object
    initialize = function(config = NULL) {
      super$initialize(config)
      if (!dir.exists(self$config$file_cache_dir)) {
        dir.create(self$config$file_cache_dir, recursive = TRUE)
      }
    },

    #' @description Get value from cache
    #' @param key Cache key
    #' @return Value or NULL
    get = function(key) {
      file_path <- private$key_to_path(key)
      if (!file.exists(file_path)) {
        self$stats$record_miss()
        return(NULL)
      }

      tryCatch({
        entry <- readRDS(file_path)
        if (entry$is_expired()) {
          file.remove(file_path)
          self$stats$record_miss()
          return(NULL)
        }
        self$stats$record_hit()
        entry$value
      }, error = function(e) {
        self$stats$record_miss()
        NULL
      })
    },

    #' @description Set value in cache
    #' @param key Cache key
    #' @param value Value to cache
    #' @param ttl Time to live
    set = function(key, value, ttl = NULL) {
      ttl <- ttl %||% self$config$file_ttl_seconds
      file_path <- private$key_to_path(key)
      entry <- CacheEntry$new(value, ttl)

      tryCatch({
        if (self$config$compression) {
          saveRDS(entry, file_path, compress = TRUE)
        } else {
          saveRDS(entry, file_path, compress = FALSE)
        }
        self$stats$record_set()
      }, error = function(e) {
        warning(sprintf("Failed to cache: %s", e$message))
      })

      invisible(NULL)
    },

    #' @description Delete key from cache
    #' @param key Cache key
    #' @return Logical success
    delete = function(key) {
      file_path <- private$key_to_path(key)
      if (file.exists(file_path)) {
        file.remove(file_path)
        self$stats$record_delete()
        return(TRUE)
      }
      FALSE
    },

    #' @description Check if key exists
    #' @param key Cache key
    #' @return Logical
    exists = function(key) {
      file_path <- private$key_to_path(key)
      if (!file.exists(file_path)) {
        return(FALSE)
      }
      tryCatch({
        entry <- readRDS(file_path)
        if (entry$is_expired()) {
          file.remove(file_path)
          return(FALSE)
        }
        TRUE
      }, error = function(e) {
        FALSE
      })
    },

    #' @description Clear cache
    clear = function() {
      files <- list.files(self$config$file_cache_dir, pattern = "\\.rds$", full.names = TRUE)
      file.remove(files)
      invisible(NULL)
    },

    #' @description Get cache size
    #' @return Integer
    size = function() {
      length(list.files(self$config$file_cache_dir, pattern = "\\.rds$"))
    },

    #' @description Cleanup expired entries
    #' @return Integer count removed
    cleanup_expired = function() {
      count <- 0
      files <- list.files(self$config$file_cache_dir, pattern = "\\.rds$", full.names = TRUE)
      for (file_path in files) {
        tryCatch({
          entry <- readRDS(file_path)
          if (entry$is_expired()) {
            file.remove(file_path)
            count <- count + 1
          }
        }, error = function(e) {
          file.remove(file_path)
          count <<- count + 1
        })
      }
      count
    }
  ),

  private = list(
    key_to_path = function(key) {
      # Hash the key for safe filenames
      hash <- digest::digest(paste0(self$config$prefix, key), algo = "md5")
      file.path(self$config$file_cache_dir, paste0(hash, ".rds"))
    }
  )
)

# =============================================================================
# Vector Cache (Specialized)
# =============================================================================

#' Vector Cache
#'
#' @description Specialized cache for vector search results
#'
#' Features:
#' - Query result caching
#' - Vector embedding caching
#' - Automatic cache invalidation
#'
#' @export
VectorCache <- R6::R6Class(
  "VectorCache",
  public = list(
    #' @field prefix Cache key prefix
    prefix = "vec:",

    #' @description Create a new VectorCache
    #' @param cache Base cache backend
    #' @param prefix Key prefix (default: "vec:")
    initialize = function(cache, prefix = "vec:") {
      private$cache <- cache
      self$prefix <- prefix
    },

    #' @description Get cached search results
    #' @param collection Collection name
    #' @param query Query vector
    #' @param filter Filter conditions
    #' @param limit Result limit
    #' @return Cached results or NULL
    get_search_results = function(collection, query, filter = NULL, limit = 10) {
      query_hash <- private$hash_query(query, filter, limit)
      key <- private$query_key(collection, query_hash)
      private$cache$get(key)
    },

    #' @description Cache search results
    #' @param collection Collection name
    #' @param query Query vector
    #' @param results Search results
    #' @param filter Filter conditions
    #' @param limit Result limit
    #' @param ttl Time to live (default: 300)
    set_search_results = function(collection, query, results, filter = NULL, limit = 10, ttl = 300) {
      query_hash <- private$hash_query(query, filter, limit)
      key <- private$query_key(collection, query_hash)
      private$cache$set(key, results, ttl)
    },

    #' @description Get cached vector
    #' @param collection Collection name
    #' @param vector_id Vector ID
    #' @return Cached vector data or NULL
    get_vector = function(collection, vector_id) {
      key <- private$vector_key(collection, vector_id)
      private$cache$get(key)
    },

    #' @description Cache vector data
    #' @param collection Collection name
    #' @param vector_id Vector ID
    #' @param data Vector data
    #' @param ttl Time to live (default: 3600)
    set_vector = function(collection, vector_id, data, ttl = 3600) {
      key <- private$vector_key(collection, vector_id)
      private$cache$set(key, data, ttl)
    },

    #' @description Invalidate cached vector
    #' @param collection Collection name
    #' @param vector_id Vector ID
    invalidate_vector = function(collection, vector_id) {
      key <- private$vector_key(collection, vector_id)
      private$cache$delete(key)
    },

    #' @description Get cache statistics
    #' @return CacheStats object
    stats = function() {
      private$cache$stats
    }
  ),

  private = list(
    cache = NULL,

    query_key = function(collection, query_hash) {
      paste0(self$prefix, "q:", collection, ":", query_hash)
    },

    vector_key = function(collection, vector_id) {
      paste0(self$prefix, "v:", collection, ":", vector_id)
    },

    hash_query = function(query, filter, limit) {
      # Round query values for better cache hits
      if (is.numeric(query)) {
        query <- round(query, 6)
      }

      data <- list(q = query, f = filter, l = limit)
      digest::digest(data, algo = "md5")
    }
  )
)

# =============================================================================
# Cache Factory
# =============================================================================

#' Create Cache
#'
#' @description Factory function to create cache backend
#' @param config CacheConfig object or NULL for defaults
#' @return Cache object
#' @export
create_cache <- function(config = NULL) {
  config <- config %||% CacheConfig$new()

  if (config$backend == CacheBackend$NONE) {
    NoCache$new(config)
  } else if (config$backend == CacheBackend$MEMORY) {
    MemoryCache$new(config)
  } else if (config$backend == CacheBackend$FILE) {
    FileCache$new(config)
  } else {
    stop(sprintf("Unknown cache backend: %s", config$backend))
  }
}

#' Create Vector Cache
#'
#' @description Create a VectorCache with specified backend
#' @param backend Backend type: "memory", "file", or "none"
#' @param ... Additional config options
#' @return VectorCache object
#' @export
create_vector_cache <- function(backend = "memory", ...) {
  config <- CacheConfig$new(backend = backend, ...)
  base_cache <- create_cache(config)
  VectorCache$new(base_cache)
}

#' Null coalescing for cache.R
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a
