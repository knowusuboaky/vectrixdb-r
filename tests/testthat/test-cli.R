# Tests for cli.R - CLI Tools for VectrixDB

test_that("CLIConfig initializes with defaults", {
  config <- CLIConfig$new()

  expect_true(config$verbose)
  expect_true(config$color)
  expect_equal(config$data_dir, get_data_path())
})

test_that("CLIConfig initializes with custom values", {
  custom_dir <- file.path(tempdir(), "cli_custom_path")
  config <- CLIConfig$new(
    verbose = FALSE,
    color = FALSE,
    data_dir = custom_dir
  )

  expect_false(config$verbose)
  expect_false(config$color)
  expect_equal(
    normalizePath(config$data_dir, mustWork = FALSE),
    normalizePath(custom_dir, mustWork = FALSE)
  )
})

test_that("set_cli_config and get_cli_config work", {
  # Create custom config
  config <- CLIConfig$new(verbose = FALSE, data_dir = tempdir())

  # Set it
  set_cli_config(config)

  # Get it back
  retrieved <- get_cli_config()

  expect_false(retrieved$verbose)
  expect_equal(
    normalizePath(retrieved$data_dir, mustWork = FALSE),
    normalizePath(tempdir(), mustWork = FALSE)
  )

  # Reset to default for other tests
  set_cli_config(CLIConfig$new())
})

test_that("vdb_list returns empty for nonexistent directory", {
  # Suppress messages for testing
  old_config <- get_cli_config()
  set_cli_config(CLIConfig$new(verbose = FALSE))

  result <- vdb_list(data_dir = "/nonexistent/path/12345")

  expect_equal(length(result), 0)

  set_cli_config(old_config)
})

test_that("vdb_list returns collections in directory", {
  old_config <- get_cli_config()
  set_cli_config(CLIConfig$new(verbose = FALSE))

  # Create temp directory with fake collections
  temp_data <- file.path(tempdir(), "vdb_test_list")
  dir.create(temp_data, showWarnings = FALSE)
  dir.create(file.path(temp_data, "collection1"), showWarnings = FALSE)
  dir.create(file.path(temp_data, "collection2"), showWarnings = FALSE)

  result <- vdb_list(data_dir = temp_data)

  expect_true("collection1" %in% result)
  expect_true("collection2" %in% result)

  # Cleanup
  unlink(temp_data, recursive = TRUE)
  set_cli_config(old_config)
})

test_that("vdb_create creates a new collection", {
  skip_if_not_installed("jsonlite")

  old_config <- get_cli_config()
  set_cli_config(CLIConfig$new(verbose = FALSE))

  temp_data <- file.path(tempdir(), "vdb_test_create")
  dir.create(temp_data, showWarnings = FALSE)

  db <- vdb_create("test_collection", data_dir = temp_data)

  expect_true(inherits(db, "Vectrix"))
  expect_equal(db$name, "test_collection")
  expect_equal(db$count(), 0)

  # Cleanup
  unlink(temp_data, recursive = TRUE)
  set_cli_config(old_config)
})

test_that("vdb_open opens existing collection", {
  skip_if_not_installed("jsonlite")

  old_config <- get_cli_config()
  set_cli_config(CLIConfig$new(verbose = FALSE))

  temp_data <- file.path(tempdir(), "vdb_test_open")
  dir.create(temp_data, showWarnings = FALSE)

  # Create first - add multiple docs to avoid TF-IDF single-doc issues
  db1 <- vdb_create("my_collection", data_dir = temp_data)
  db1$add(c(
    "This is the first test document with multiple words for embedding",
    "This is the second test document with different content for variety"
  ))

  # Open
  db2 <- vdb_open("my_collection", data_dir = temp_data)

  expect_true(inherits(db2, "Vectrix"))
  expect_equal(db2$name, "my_collection")
  expect_equal(db2$count(), 2)

  # Cleanup
  unlink(temp_data, recursive = TRUE)
  set_cli_config(old_config)
})

test_that("vdb_add adds documents", {
  skip_if_not_installed("jsonlite")

  old_config <- get_cli_config()
  set_cli_config(CLIConfig$new(verbose = FALSE))

  temp_data <- file.path(tempdir(), "vdb_test_add")
  dir.create(temp_data, showWarnings = FALSE)

  db <- vdb_create("add_test", data_dir = temp_data)

  vdb_add(db, c(
    "This is the first document with multiple words",
    "This is the second document with more words to test",
    "This is the third document for complete testing purposes"
  ))

  expect_equal(db$count(), 3)

  # Cleanup
  unlink(temp_data, recursive = TRUE)
  set_cli_config(old_config)
})

test_that("vdb_add works with collection name string", {
  skip_if_not_installed("jsonlite")

  old_config <- get_cli_config()
  config <- CLIConfig$new(verbose = FALSE)
  temp_data <- file.path(tempdir(), "vdb_test_add_str")
  config$data_dir <- temp_data
  set_cli_config(config)

  dir.create(temp_data, showWarnings = FALSE)

  # Create collection first
  db <- vdb_create("string_test", data_dir = temp_data)

  # Add using string name (via config's data_dir) - use multiple docs
  vdb_add("string_test", c(
    "This is a longer new document with multiple words for testing",
    "This is a second document to ensure proper TF-IDF processing"
  ))

  # Verify
  db2 <- vdb_open("string_test", data_dir = temp_data)
  expect_equal(db2$count(), 2)

  # Cleanup
  unlink(temp_data, recursive = TRUE)
  set_cli_config(old_config)
})

test_that("vdb_search returns results", {
  skip_if_not_installed("jsonlite")

  old_config <- get_cli_config()
  set_cli_config(CLIConfig$new(verbose = FALSE))

  temp_data <- file.path(tempdir(), "vdb_test_search")
  dir.create(temp_data, showWarnings = FALSE)

  db <- vdb_create("search_test", data_dir = temp_data)
  db$add(c(
    "Machine learning is great",
    "Deep learning uses neural networks",
    "Cooking recipes are delicious"
  ))

  results <- vdb_search(db, "machine learning", limit = 2, show = FALSE)

  expect_true(!is.null(results))
  expect_true(length(results$items) > 0)

  # Cleanup
  unlink(temp_data, recursive = TRUE)
  set_cli_config(old_config)
})

test_that("vdb_get retrieves documents by ID", {
  skip_if_not_installed("jsonlite")

  old_config <- get_cli_config()
  set_cli_config(CLIConfig$new(verbose = FALSE))

  temp_data <- file.path(tempdir(), "vdb_test_get")
  dir.create(temp_data, showWarnings = FALSE)

  db <- vdb_create("get_test", data_dir = temp_data)
  db$add(c(
    "This is a test document with multiple words for retrieval",
    "This is a second document to ensure proper TF-IDF processing"
  ), ids = c("doc1", "doc2"))

  result <- vdb_get(db, "doc1")

  expect_true(length(result) > 0)

  # Cleanup
  unlink(temp_data, recursive = TRUE)
  set_cli_config(old_config)
})

test_that("vdb_delete_docs removes documents", {
  skip_if_not_installed("jsonlite")

  old_config <- get_cli_config()
  set_cli_config(CLIConfig$new(verbose = FALSE))

  temp_data <- file.path(tempdir(), "vdb_test_delete_docs")
  dir.create(temp_data, showWarnings = FALSE)

  db <- vdb_create("delete_test", data_dir = temp_data)
  db$add(c(
    "This is the first document to be potentially deleted",
    "This is the second document that we might remove",
    "This is the third document for deletion testing purposes"
  ), ids = c("a", "b", "c"))

  expect_equal(db$count(), 3)

  vdb_delete_docs(db, c("b"))

  expect_equal(db$count(), 2)

  # Cleanup
  unlink(temp_data, recursive = TRUE)
  set_cli_config(old_config)
})

test_that("vdb_info returns collection info", {
  skip_if_not_installed("jsonlite")

  old_config <- get_cli_config()
  set_cli_config(CLIConfig$new(verbose = FALSE))

  temp_data <- file.path(tempdir(), "vdb_test_info")
  dir.create(temp_data, showWarnings = FALSE)

  db <- vdb_create("info_test", data_dir = temp_data)
  db$add(c(
    "This is some longer text with multiple words for the info test",
    "This is a second document to ensure TF-IDF works properly"
  ))

  info <- vdb_info(db)

  expect_true(is.list(info))
  expect_equal(info$name, "info_test")
  expect_equal(info$count, 2)
  expect_true("dimension" %in% names(info))
  expect_true("model" %in% names(info))

  # Cleanup
  unlink(temp_data, recursive = TRUE)
  set_cli_config(old_config)
})

test_that("vdb_stats returns statistics", {
  skip_if_not_installed("jsonlite")

  old_config <- get_cli_config()
  set_cli_config(CLIConfig$new(verbose = FALSE))

  temp_data <- file.path(tempdir(), "vdb_test_stats")
  dir.create(temp_data, showWarnings = FALSE)

  db <- vdb_create("stats_test", data_dir = temp_data)
  db$add(c(
    "This is the first document with many words for statistics testing",
    "This is the second document to check the stats function properly"
  ))

  stats <- vdb_stats(db)

  expect_true(is.list(stats))
  expect_equal(stats$name, "stats_test")
  expect_equal(stats$document_count, 2)
  expect_true("memory_mb" %in% names(stats))

  # Cleanup
  unlink(temp_data, recursive = TRUE)
  set_cli_config(old_config)
})

test_that("vdb_export creates export file", {
  skip_if_not_installed("jsonlite")

  old_config <- get_cli_config()
  set_cli_config(CLIConfig$new(verbose = FALSE))

  temp_data <- file.path(tempdir(), "vdb_test_export")
  dir.create(temp_data, showWarnings = FALSE)

  db <- vdb_create("export_test", data_dir = temp_data)
  db$add(c(
    "This is a longer document with multiple words to export properly",
    "This is a second document to ensure TF-IDF processing works well"
  ))

  export_path <- file.path(temp_data, "export.json")
  result <- vdb_export(db, export_path)

  expect_true(result)
  expect_true(file.exists(export_path))

  # Verify content
  content <- jsonlite::fromJSON(export_path)
  expect_equal(content$name, "export_test")

  # Cleanup
  unlink(temp_data, recursive = TRUE)
  set_cli_config(old_config)
})

test_that("vdb_import imports from text file", {
  skip_if_not_installed("jsonlite")

  old_config <- get_cli_config()
  set_cli_config(CLIConfig$new(verbose = FALSE))

  temp_data <- file.path(tempdir(), "vdb_test_import")
  dir.create(temp_data, showWarnings = FALSE)

  # Create test file
  import_file <- file.path(temp_data, "docs.txt")
  writeLines(c("Line 1", "Line 2", "Line 3"), import_file)

  db <- vdb_create("import_test", data_dir = temp_data)

  vdb_import(db, import_file)

  expect_equal(db$count(), 3)

  # Cleanup
  unlink(temp_data, recursive = TRUE)
  set_cli_config(old_config)
})

test_that("vdb_import handles missing file", {
  skip_if_not_installed("jsonlite")

  old_config <- get_cli_config()
  set_cli_config(CLIConfig$new(verbose = FALSE))

  temp_data <- file.path(tempdir(), "vdb_test_import_missing")
  dir.create(temp_data, showWarnings = FALSE)

  db <- vdb_create("import_missing_test", data_dir = temp_data)

  # Should not error, just warn
  vdb_import(db, "/nonexistent/file.txt")

  expect_equal(db$count(), 0)

  # Cleanup
  unlink(temp_data, recursive = TRUE)
  set_cli_config(old_config)
})

test_that("vdb_add_dir imports from directory", {
  skip_if_not_installed("jsonlite")

  old_config <- get_cli_config()
  set_cli_config(CLIConfig$new(verbose = FALSE))

  temp_data <- file.path(tempdir(), "vdb_test_add_dir")
  dir.create(temp_data, showWarnings = FALSE)

  # Create test files
  docs_dir <- file.path(temp_data, "docs")
  dir.create(docs_dir, showWarnings = FALSE)

  writeLines("This is the full content of file one with multiple words for embedding", file.path(docs_dir, "file1.txt"))
  writeLines("This is the full content of file two with many more words for testing", file.path(docs_dir, "file2.txt"))

  db <- vdb_create("dir_test", data_dir = temp_data)

  vdb_add_dir(db, docs_dir, pattern = "\\.txt$")

  expect_equal(db$count(), 2)

  # Cleanup
  unlink(temp_data, recursive = TRUE)
  set_cli_config(old_config)
})

test_that("vdb_add_dir handles missing directory", {
  skip_if_not_installed("jsonlite")

  old_config <- get_cli_config()
  set_cli_config(CLIConfig$new(verbose = FALSE))

  temp_data <- file.path(tempdir(), "vdb_test_add_dir_missing")
  dir.create(temp_data, showWarnings = FALSE)

  db <- vdb_create("dir_missing_test", data_dir = temp_data)

  # Should not error
  vdb_add_dir(db, "/nonexistent/directory")

  expect_equal(db$count(), 0)

  # Cleanup
  unlink(temp_data, recursive = TRUE)
  set_cli_config(old_config)
})

test_that("vdb_delete deletes collection without confirm", {
  old_config <- get_cli_config()
  set_cli_config(CLIConfig$new(verbose = FALSE))

  temp_data <- file.path(tempdir(), "vdb_test_delete_col")
  dir.create(temp_data, showWarnings = FALSE)

  # Create a fake collection directory
  col_path <- file.path(temp_data, "to_delete")
  dir.create(col_path, showWarnings = FALSE)
  writeLines("test", file.path(col_path, "test.txt"))

  expect_true(dir.exists(col_path))

  result <- vdb_delete("to_delete", data_dir = temp_data, confirm = FALSE)

  expect_true(result)
  expect_false(dir.exists(col_path))

  # Cleanup
  unlink(temp_data, recursive = TRUE)
  set_cli_config(old_config)
})

test_that("vdb_delete returns FALSE for nonexistent collection", {
  old_config <- get_cli_config()
  set_cli_config(CLIConfig$new(verbose = FALSE))

  result <- vdb_delete("nonexistent_collection_xyz", data_dir = tempdir(), confirm = FALSE)

  expect_false(result)

  set_cli_config(old_config)
})
