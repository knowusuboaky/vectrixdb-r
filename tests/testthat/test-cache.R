# Tests for cache.R - Memory Cache, File Cache, Vector Cache

test_that("CacheConfig initializes correctly", {
  config <- CacheConfig$new(
    backend = "memory",
    memory_max_size = 1000,
    memory_ttl_seconds = 300
  )

  expect_equal(config$backend, "memory")
  expect_equal(config$memory_max_size, 1000)
  expect_equal(config$memory_ttl_seconds, 300)
})

test_that("CacheStats tracks operations", {
  stats <- CacheStats$new()

  expect_equal(stats$hits, 0)
  expect_equal(stats$misses, 0)

  stats$record_hit()
  stats$record_hit()
  stats$record_miss()

  expect_equal(stats$hits, 2)
  expect_equal(stats$misses, 1)
  expect_equal(stats$hit_rate(), 2/3)

  stats_list <- stats$to_list()
  expect_true(is.list(stats_list))
  expect_equal(stats_list$hits, 2)
})

test_that("CacheEntry tracks expiration", {
  entry <- CacheEntry$new(value = "test", ttl = 1)

  expect_equal(entry$value, "test")
  expect_false(entry$is_expired())

  # Wait for expiration (only if running slow tests)
  # Sys.sleep(1.1)
  # expect_true(entry$is_expired())
})

test_that("NoCache returns NULL for all operations", {
  cache <- NoCache$new()

  cache$set("key", "value")
  expect_null(cache$get("key"))
  expect_false(cache$exists("key"))
  expect_false(cache$delete("key"))
  expect_equal(cache$size(), 0)
})

test_that("MemoryCache basic operations work", {
  cache <- MemoryCache$new()

  # Set and get
  cache$set("key1", "value1")
  expect_equal(cache$get("key1"), "value1")
  expect_equal(cache$size(), 1)

  # Exists
  expect_true(cache$exists("key1"))
  expect_false(cache$exists("nonexistent"))

  # Delete
  expect_true(cache$delete("key1"))
  expect_null(cache$get("key1"))
  expect_equal(cache$size(), 0)
})

test_that("MemoryCache respects max size (LRU eviction)", {
  config <- CacheConfig$new(memory_max_size = 3)
  cache <- MemoryCache$new(config)

  cache$set("a", 1)
  cache$set("b", 2)
  cache$set("c", 3)
  expect_equal(cache$size(), 3)

  # Adding 4th should evict oldest (a)
  cache$set("d", 4)
  expect_equal(cache$size(), 3)
  expect_null(cache$get("a"))
  expect_equal(cache$get("d"), 4)
})

test_that("MemoryCache LRU access order works", {
  config <- CacheConfig$new(memory_max_size = 3)
  cache <- MemoryCache$new(config)

  cache$set("a", 1)
  cache$set("b", 2)
  cache$set("c", 3)

  # Access 'a' to make it recently used
  cache$get("a")

  # Add 'd' - should evict 'b' (least recently used)
  cache$set("d", 4)

  expect_equal(cache$get("a"), 1)  # Still there
  expect_null(cache$get("b"))       # Evicted
  expect_equal(cache$get("c"), 3)
  expect_equal(cache$get("d"), 4)
})

test_that("MemoryCache clear works", {
  cache <- MemoryCache$new()

  cache$set("a", 1)
  cache$set("b", 2)
  expect_equal(cache$size(), 2)

  cache$clear()
  expect_equal(cache$size(), 0)
})

test_that("MemoryCache get_many works", {
  cache <- MemoryCache$new()

  cache$set("a", 1)
  cache$set("b", 2)
  cache$set("c", 3)

  result <- cache$get_many(c("a", "b", "missing"))
  expect_equal(result$a, 1)
  expect_equal(result$b, 2)
  expect_false("missing" %in% names(result))
})

test_that("MemoryCache set_many works", {
  cache <- MemoryCache$new()

  cache$set_many(list(a = 1, b = 2, c = 3))

  expect_equal(cache$get("a"), 1)
  expect_equal(cache$get("b"), 2)
  expect_equal(cache$get("c"), 3)
  expect_equal(cache$size(), 3)
})

test_that("MemoryCache tracks stats correctly", {
  cache <- MemoryCache$new()

  cache$set("a", 1)
  cache$get("a")  # Hit
  cache$get("missing")  # Miss

  expect_equal(cache$stats$hits, 1)
  expect_equal(cache$stats$misses, 1)
  expect_equal(cache$stats$sets, 1)
})

test_that("FileCache basic operations work", {
  # Use temp directory
  config <- CacheConfig$new(
    backend = "file",
    file_cache_dir = file.path(tempdir(), "vectrix_test_cache")
  )
  cache <- FileCache$new(config)

  # Clean up first
  cache$clear()

  # Set and get
  cache$set("key1", list(data = "test"))
  result <- cache$get("key1")
  expect_equal(result$data, "test")

  # Exists
  expect_true(cache$exists("key1"))

  # Delete
  expect_true(cache$delete("key1"))
  expect_null(cache$get("key1"))

  # Clean up
  cache$clear()
})

test_that("VectorCache query caching works", {
  base_cache <- MemoryCache$new()
  vec_cache <- VectorCache$new(base_cache)

  # Cache search results
  query <- c(0.1, 0.2, 0.3)
  results <- list(list(id = "a", score = 0.9), list(id = "b", score = 0.8))

  vec_cache$set_search_results("test_collection", query, results)

  cached <- vec_cache$get_search_results("test_collection", query)
  expect_equal(length(cached), 2)
  expect_equal(cached[[1]]$id, "a")
})

test_that("VectorCache vector caching works", {
  base_cache <- MemoryCache$new()
  vec_cache <- VectorCache$new(base_cache)

  vec_data <- list(id = "vec1", vector = c(1, 2, 3), text = "test")

  vec_cache$set_vector("collection", "vec1", vec_data)
  cached <- vec_cache$get_vector("collection", "vec1")

  expect_equal(cached$id, "vec1")
  expect_equal(cached$vector, c(1, 2, 3))
})

test_that("VectorCache invalidation works", {
  base_cache <- MemoryCache$new()
  vec_cache <- VectorCache$new(base_cache)

  vec_cache$set_vector("col", "v1", list(data = 1))
  expect_true(!is.null(vec_cache$get_vector("col", "v1")))

  vec_cache$invalidate_vector("col", "v1")
  expect_null(vec_cache$get_vector("col", "v1"))
})

test_that("create_cache factory works", {
  # Memory cache
  config1 <- CacheConfig$new(backend = "memory")
  cache1 <- create_cache(config1)
  expect_true(inherits(cache1, "MemoryCache"))

  # No cache
  config2 <- CacheConfig$new(backend = "none")
  cache2 <- create_cache(config2)
  expect_true(inherits(cache2, "NoCache"))
})

test_that("create_vector_cache factory works", {
  vec_cache <- create_vector_cache("memory")
  expect_true(inherits(vec_cache, "VectorCache"))
})
