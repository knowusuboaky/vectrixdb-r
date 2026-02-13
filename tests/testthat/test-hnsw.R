# Tests for hnsw.R - HNSW Index for Approximate Nearest Neighbor Search

test_that("DistanceMetric contains correct values", {
  expect_equal(DistanceMetric$COSINE, "angular")
  expect_equal(DistanceMetric$EUCLIDEAN, "euclidean")
  expect_equal(DistanceMetric$DOT, "dot")
  expect_equal(DistanceMetric$MANHATTAN, "manhattan")
})

test_that("HNSWIndex initializes correctly", {
  index <- HNSWIndex$new(dimension = 128, metric = "angular")

  expect_equal(index$dimension, 128)
  expect_equal(index$metric, "angular")
  expect_equal(index$n_trees, 50)
  expect_equal(index$size(), 0)
})

test_that("HNSWIndex initializes with custom parameters", {
  index <- HNSWIndex$new(
    dimension = 64,
    metric = "euclidean",
    n_trees = 100,
    search_k = 500
  )

  expect_equal(index$dimension, 64)
  expect_equal(index$metric, "euclidean")
  expect_equal(index$n_trees, 100)
  expect_equal(index$search_k, 500)
})

test_that("HNSWIndex add_items works with single vector", {
  index <- HNSWIndex$new(dimension = 4)

  vec <- c(1.0, 2.0, 3.0, 4.0)
  index$add_items(ids = c("a"), vectors = vec)

  expect_equal(index$size(), 1)
  expect_equal(index$get_ids(), "a")
})

test_that("HNSWIndex add_items works with matrix", {
  index <- HNSWIndex$new(dimension = 3)

  vectors <- matrix(c(
    1, 0, 0,
    0, 1, 0,
    0, 0, 1
  ), nrow = 3, byrow = TRUE)

  index$add_items(ids = c("x", "y", "z"), vectors = vectors)

  expect_equal(index$size(), 3)
  expect_true(all(c("x", "y", "z") %in% index$get_ids()))
})

test_that("HNSWIndex add_items validates dimensions", {
  index <- HNSWIndex$new(dimension = 4)

  wrong_dim <- matrix(c(1, 2, 3), nrow = 1)
  expect_error(
    index$add_items(ids = c("a"), vectors = wrong_dim),
    "dimension mismatch"
  )
})

test_that("HNSWIndex add_items validates ID count", {
  index <- HNSWIndex$new(dimension = 3)

  vectors <- matrix(rnorm(6), nrow = 2)
  expect_error(
    index$add_items(ids = c("a"), vectors = vectors),
    "must match"
  )
})

test_that("HNSWIndex get_vector returns correct vector", {
  index <- HNSWIndex$new(dimension = 3)

  vec <- c(1.5, 2.5, 3.5)
  index$add_items(ids = c("test"), vectors = vec)

  result <- index$get_vector("test")
  expect_equal(result, vec)
})

test_that("HNSWIndex get_vector returns NULL for missing ID", {
  index <- HNSWIndex$new(dimension = 3)
  expect_null(index$get_vector("nonexistent"))
})

test_that("HNSWIndex search returns results", {
  index <- HNSWIndex$new(dimension = 3)

  vectors <- matrix(c(
    1, 0, 0,
    0, 1, 0,
    0, 0, 1
  ), nrow = 3, byrow = TRUE)

  index$add_items(ids = c("x", "y", "z"), vectors = vectors)
  index$build()

  query <- c(1, 0, 0)
  results <- index$search(query, k = 2)

  expect_true(is.data.frame(results))
  expect_true("id" %in% names(results))
  expect_true("distance" %in% names(results))
  expect_equal(nrow(results), 2)
  expect_equal(results$id[1], "x")  # Closest to query
})

test_that("HNSWIndex search returns empty for empty index", {
  index <- HNSWIndex$new(dimension = 3)

  results <- index$search(c(1, 0, 0), k = 5)

  expect_true(is.data.frame(results))
  expect_equal(nrow(results), 0)
})

test_that("HNSWIndex search limits k to available items", {
  index <- HNSWIndex$new(dimension = 3)

  index$add_items(ids = c("a", "b"), vectors = matrix(rnorm(6), nrow = 2))
  index$build()

  results <- index$search(rnorm(3), k = 10)
  expect_equal(nrow(results), 2)
})

test_that("HNSWIndex remove_items works", {
  index <- HNSWIndex$new(dimension = 3)

  index$add_items(ids = c("a", "b", "c"), vectors = matrix(rnorm(9), nrow = 3))
  expect_equal(index$size(), 3)

  index$remove_items(c("b"))
  expect_equal(index$size(), 2)
  expect_null(index$get_vector("b"))
  expect_true(!is.null(index$get_vector("a")))
})

test_that("HNSWIndex clear works", {
  index <- HNSWIndex$new(dimension = 3)

  index$add_items(ids = c("a", "b"), vectors = matrix(rnorm(6), nrow = 2))
  expect_equal(index$size(), 2)

  index$clear()
  expect_equal(index$size(), 0)
  expect_equal(length(index$get_ids()), 0)
})

test_that("HNSWIndex save and load works", {
  index <- HNSWIndex$new(dimension = 4, metric = "euclidean")

  vectors <- matrix(rnorm(12), nrow = 3)
  index$add_items(ids = c("a", "b", "c"), vectors = vectors)
  index$build()

  # Save
  temp_path <- file.path(tempdir(), "test_hnsw.rds")
  index$save(temp_path)

  expect_true(file.exists(temp_path))

  # Load into new index
  new_index <- HNSWIndex$new(dimension = 1)  # Will be overwritten
  new_index$load(temp_path)

  expect_equal(new_index$dimension, 4)
  expect_equal(new_index$metric, "euclidean")
  expect_equal(new_index$size(), 3)
  expect_equal(new_index$get_vector("a"), vectors[1, ])

  # Clean up
  unlink(temp_path)
  unlink(paste0(temp_path, ".annoy"))
})

test_that("HNSWIndex brute force search works with cosine distance", {
  index <- HNSWIndex$new(dimension = 3, metric = "cosine")

  # Use normalized vectors for cosine
  v1 <- c(1, 0, 0)
  v2 <- c(0.7071, 0.7071, 0)
  v3 <- c(0, 1, 0)

  index$add_items(ids = c("a", "b", "c"), vectors = rbind(v1, v2, v3))

  # Query close to v1
  results <- index$search(c(0.9, 0.1, 0), k = 3)

  expect_equal(nrow(results), 3)
  # First result should be "a" (closest to query)
  expect_equal(results$id[1], "a")
})

test_that("HNSWIndex brute force search works with euclidean distance", {
  index <- HNSWIndex$new(dimension = 2, metric = "euclidean")

  # Simple 2D points
  index$add_items(
    ids = c("origin", "far"),
    vectors = rbind(c(0, 0), c(10, 10))
  )

  # Query near origin
  results <- index$search(c(0.1, 0.1), k = 2)

  expect_equal(results$id[1], "origin")
  expect_true(results$distance[1] < results$distance[2])
})

test_that("HNSWIndex brute force search works with manhattan distance", {
  index <- HNSWIndex$new(dimension = 2, metric = "manhattan")

  index$add_items(
    ids = c("a", "b"),
    vectors = rbind(c(0, 0), c(1, 1))
  )

  results <- index$search(c(0.5, 0), k = 2)

  # Manhattan distance to (0,0) is 0.5, to (1,1) is 1.5
  expect_equal(results$id[1], "a")
})

test_that("create_hnsw_index factory works", {
  index <- create_hnsw_index(dimension = 128, metric = "euclidean", n_trees = 25)

  expect_true(inherits(index, "HNSWIndex"))
  expect_equal(index$dimension, 128)
  expect_equal(index$metric, "euclidean")
  expect_equal(index$n_trees, 25)
})

test_that("load_hnsw_index factory works", {
  # Create and save
  index <- create_hnsw_index(dimension = 8)
  index$add_items(ids = c("test"), vectors = rnorm(8))
  index$build()

  temp_path <- file.path(tempdir(), "test_load_hnsw.rds")
  index$save(temp_path)

  # Load via factory
  loaded <- load_hnsw_index(temp_path)

  expect_true(inherits(loaded, "HNSWIndex"))
  expect_equal(loaded$dimension, 8)
  expect_equal(loaded$size(), 1)

  # Clean up
  unlink(temp_path)
  unlink(paste0(temp_path, ".annoy"))
})

test_that("HNSWIndex auto-builds on search if not built", {
  index <- HNSWIndex$new(dimension = 3)

  index$add_items(ids = c("a"), vectors = c(1, 2, 3))
  # Don't call build()

  # Search should auto-build
  results <- index$search(c(1, 2, 3), k = 1)

  expect_equal(nrow(results), 1)
})
