test_that("Vectrix basic operations work", {
  # Create a new Vectrix instance
  db <- Vectrix$new("test_collection")

  # Should start empty
  expect_equal(db$count(), 0)

  # Add some documents
  db$add(c("The quick brown fox", "jumps over the lazy dog", "Hello world"))

  # Should have 3 documents
  expect_equal(db$count(), 3)

  # Clear should work
  db$clear()
  expect_equal(db$count(), 0)
})

test_that("Result class works correctly", {
  result <- Result$new(
    id = "doc1",
    text = "Test document",
    score = 0.95,
    metadata = list(source = "test")
  )

  expect_equal(result$id, "doc1")
  expect_equal(result$text, "Test document")
  expect_equal(result$score, 0.95)
  expect_equal(result$metadata$source, "test")
})

test_that("Results class works correctly", {
  items <- list(
    Result$new("doc1", "First", 0.9, list()),
    Result$new("doc2", "Second", 0.8, list()),
    Result$new("doc3", "Third", 0.7, list())
  )

  results <- Results$new(
    items = items,
    query = "test query",
    mode = "hybrid",
    time_ms = 10.5
  )

  expect_equal(results$length(), 3)
  expect_equal(results$top()$id, "doc1")
  expect_equal(results$texts(), c("First", "Second", "Third"))
  expect_equal(results$ids(), c("doc1", "doc2", "doc3"))
  expect_equal(results$scores(), c(0.9, 0.8, 0.7))
})

test_that("Filter class works correctly", {
  filter <- Filter$new()
  filter$eq("category", "tech")
  filter$gt("score", 0.5)

  conditions <- filter$to_list()

  expect_equal(conditions$category$op, "eq")
  expect_equal(conditions$category$value, "tech")
  expect_equal(conditions$score$op, "gt")
  expect_equal(conditions$score$value, 0.5)
})

test_that("Collection basic operations work", {
  collection <- Collection$new(
    name = "test",
    dimension = 3,
    metric = "cosine"
  )

  expect_equal(collection$count(), 0)

  # Add vectors
  collection$add(
    ids = c("a", "b"),
    vectors = matrix(c(1, 0, 0, 0, 1, 0), nrow = 2, byrow = TRUE),
    texts = c("doc a", "doc b")
  )

  expect_equal(collection$count(), 2)

  # Search
  results <- collection$search(
    query = c(1, 0, 0),
    limit = 2
  )

  expect_equal(results$length(), 2)
  expect_equal(results$top()$id, "a")  # Should match first vector

  # Delete
  collection$delete(ids = "a")
  expect_equal(collection$count(), 1)

  # Clear
  collection$clear()
  expect_equal(collection$count(), 0)
})

test_that("Utility functions work correctly", {
  # generate_id should be deterministic
  id1 <- generate_id("test text")
  id2 <- generate_id("test text")
  expect_equal(id1, id2)

  # Different texts should have different IDs
  id3 <- generate_id("different text")
  expect_false(id1 == id3)

  # cosine_similarity
  v1 <- c(1, 0, 0)
  v2 <- matrix(c(1, 0, 0, 0, 1, 0), nrow = 2, byrow = TRUE)
  sims <- cosine_similarity(v1, v2)
  expect_equal(sims[1], 1.0)
  expect_equal(sims[2], 0.0)
})

test_that("RRF fusion works correctly", {
  rankings <- list(
    c("a", "b", "c"),
    c("b", "a", "d")
  )

  scores <- rrf_fusion(rankings, k = 60)

  # "a" and "b" should have highest scores (appear in both)
  expect_true(scores["a"] > 0)
  expect_true(scores["b"] > 0)
  expect_true(scores["b"] > scores["c"])  # b appears in both
})
