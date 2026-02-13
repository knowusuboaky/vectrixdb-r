#' VectrixDB Command Line Interface
#'
#' @description CLI tools for VectrixDB operations
#'
#' Provides command-line style functions for:
#' - Creating and managing collections
#' - Adding and searching documents
#' - Exporting and importing data
#' - Database statistics and info
#'
#' @name cli
NULL

# =============================================================================
# CLI Configuration
# =============================================================================

#' CLI Configuration
#'
#' @description Configuration for CLI behavior
#' @export
CLIConfig <- R6::R6Class(

  "CLIConfig",
  public = list(
    #' @field verbose Print verbose output
    verbose = TRUE,
    #' @field color Use colored output
    color = TRUE,
    #' @field data_dir Default data directory
    data_dir = "./vectrixdb_data",

    #' @description Create CLI config
    #' @param verbose Verbose output
    #' @param color Colored output
    #' @param data_dir Data directory
    initialize = function(verbose = TRUE, color = TRUE, data_dir = "./vectrixdb_data") {
      self$verbose <- verbose
      self$color <- color
      self$data_dir <- data_dir
    }
  )
)

# Global CLI config
.cli_config <- new.env()
.cli_config$config <- NULL

#' Get CLI Config
#' @return CLIConfig object
#' @keywords internal
get_cli_config <- function() {
  if (is.null(.cli_config$config)) {
    .cli_config$config <- CLIConfig$new()
  }
  .cli_config$config
}

#' Set CLI Config
#' @param config CLIConfig object
#' @export
set_cli_config <- function(config) {
  .cli_config$config <- config
}

# =============================================================================
# Output Helpers
# =============================================================================

#' Print CLI Message
#' @param msg Message
#' @param type Type: "info", "success", "warning", "error"
#' @keywords internal
cli_print <- function(msg, type = "info") {
  config <- get_cli_config()
  if (!config$verbose) return(invisible(NULL))

  prefix <- switch(type,
    "info" = "[INFO] ",
    "success" = "[OK] ",
    "warning" = "[WARN] ",
    "error" = "[ERROR] ",
    ""
  )

  message(paste0(prefix, msg))
}

#' Print Table
#' @param df Data frame
#' @param max_rows Max rows to show
#' @keywords internal
cli_table <- function(df, max_rows = 20) {
  if (nrow(df) > max_rows) {
    print(head(df, max_rows))
    message(sprintf("... and %d more rows", nrow(df) - max_rows))
  } else {
    print(df)
  }
}

# =============================================================================
# Collection Management
# =============================================================================

#' List Collections
#'
#' @description List all VectrixDB collections in the data directory
#' @param data_dir Data directory path
#' @return Character vector of collection names
#' @export
#'
#' @examples
#' \dontrun{
#' vdb_list()
#' }
vdb_list <- function(data_dir = NULL) {
  config <- get_cli_config()
  data_dir <- data_dir %||% config$data_dir

  if (!dir.exists(data_dir)) {
    cli_print(sprintf("Data directory not found: %s", data_dir), "warning")
    return(character(0))
  }

  # Look for collection directories or metadata files
  items <- list.dirs(data_dir, full.names = FALSE, recursive = FALSE)

  if (length(items) == 0) {
    cli_print("No collections found", "info")
    return(character(0))
  }

  cli_print(sprintf("Found %d collection(s):", length(items)), "info")
  for (item in items) {
    message(sprintf("  - %s", item))
  }

  invisible(items)
}

#' Create Collection
#'
#' @description Create a new VectrixDB collection
#' @param name Collection name
#' @param model Embedding model
#' @param dimension Vector dimension
#' @param data_dir Data directory
#' @return Vectrix object
#' @export
#'
#' @examples
#' \dontrun{
#' db <- vdb_create("my_docs")
#' }
vdb_create <- function(name, model = "tfidf", dimension = NULL, data_dir = NULL) {
  config <- get_cli_config()
  data_dir <- data_dir %||% config$data_dir

  cli_print(sprintf("Creating collection '%s'...", name), "info")

  db <- Vectrix$new(
    name = name,
    path = data_dir,
    model = model,
    dimension = dimension
  )

  cli_print(sprintf("Collection '%s' created successfully", name), "success")
  db
}

#' Open Collection
#'
#' @description Open an existing collection
#' @param name Collection name
#' @param data_dir Data directory
#' @return Vectrix object
#' @export
#'
#' @examples
#' \dontrun{
#' db <- vdb_open("my_docs")
#' }
vdb_open <- function(name, data_dir = NULL) {
  config <- get_cli_config()
  data_dir <- data_dir %||% config$data_dir

  cli_print(sprintf("Opening collection '%s'...", name), "info")

  db <- Vectrix$new(
    name = name,
    path = data_dir
  )

  cli_print(sprintf("Collection '%s' opened (%d documents)", name, db$count()), "success")
  db
}

#' Delete Collection
#'
#' @description Delete a collection
#' @param name Collection name
#' @param data_dir Data directory
#' @param confirm Require confirmation
#' @return Logical success
#' @export
#'
#' @examples
#' \dontrun{
#' vdb_delete("my_docs")
#' }
vdb_delete <- function(name, data_dir = NULL, confirm = TRUE) {
  config <- get_cli_config()
  data_dir <- data_dir %||% config$data_dir

  collection_path <- file.path(data_dir, name)

  if (!dir.exists(collection_path)) {
    cli_print(sprintf("Collection '%s' not found", name), "warning")
    return(FALSE)
  }

  if (confirm) {
    response <- readline(sprintf("Delete collection '%s'? (y/N): ", name))
    if (tolower(response) != "y") {
      cli_print("Cancelled", "info")
      return(FALSE)
    }
  }

  unlink(collection_path, recursive = TRUE)
  cli_print(sprintf("Collection '%s' deleted", name), "success")
  TRUE
}

# =============================================================================
# Document Operations
# =============================================================================

#' Add Documents
#'
#' @description Add documents to a collection
#' @param db Vectrix object or collection name
#' @param texts Character vector of texts
#' @param metadata Optional metadata
#' @param ids Optional IDs
#' @return Vectrix object
#' @export
#'
#' @examples
#' \dontrun{
#' vdb_add(db, c("Document 1", "Document 2"))
#' vdb_add("my_docs", c("Another doc"))
#' }
vdb_add <- function(db, texts, metadata = NULL, ids = NULL) {
  if (is.character(db)) {
    db <- vdb_open(db)
  }

  n <- length(texts)
  cli_print(sprintf("Adding %d document(s)...", n), "info")

  db$add(texts, metadata, ids)

  cli_print(sprintf("Added %d document(s). Total: %d", n, db$count()), "success")
  invisible(db)
}

#' Search Collection
#'
#' @description Search a collection
#' @param db Vectrix object or collection name
#' @param query Search query
#' @param limit Number of results
#' @param mode Search mode: "dense", "sparse", "hybrid", "ultimate"
#' @param show Print results
#' @return Results object
#' @export
#'
#' @examples
#' \dontrun{
#' results <- vdb_search(db, "machine learning")
#' results <- vdb_search("my_docs", "AI", limit = 5)
#' }
vdb_search <- function(db, query, limit = 10, mode = "hybrid", show = TRUE) {
  if (is.character(db)) {
    db <- vdb_open(db)
  }

  cli_print(sprintf("Searching for: '%s' (mode=%s)...", query, mode), "info")

  results <- db$search(query, limit = limit, mode = mode)

  if (show) {
    cli_print(sprintf("Found %d result(s) in %.1f ms:",
                      length(results$items), results$time_ms), "success")

    for (i in seq_along(results$items)) {
      item <- results$items[[i]]
      text_preview <- substr(item$text, 1, 80)
      if (nchar(item$text) > 80) text_preview <- paste0(text_preview, "...")
      message(sprintf("  %d. [%.3f] %s", i, item$score, text_preview))
    }
  }

  invisible(results)
}

#' Get Document
#'
#' @description Get document by ID
#' @param db Vectrix object or collection name
#' @param ids Document ID(s)
#' @return List of Result objects
#' @export
vdb_get <- function(db, ids) {
  if (is.character(db)) {
    db <- vdb_open(db)
  }

  db$get(ids)
}

#' Delete Documents
#'
#' @description Delete documents by ID
#' @param db Vectrix object or collection name
#' @param ids Document ID(s)
#' @return Vectrix object
#' @export
vdb_delete_docs <- function(db, ids) {
  if (is.character(db)) {
    db <- vdb_open(db)
  }

  n <- length(ids)
  cli_print(sprintf("Deleting %d document(s)...", n), "info")

  db$delete(ids)

  cli_print(sprintf("Deleted %d document(s). Remaining: %d", n, db$count()), "success")
  invisible(db)
}

# =============================================================================
# Statistics and Info
# =============================================================================

#' Collection Info
#'
#' @description Get collection information
#' @param db Vectrix object or collection name
#' @return Named list of info
#' @export
#'
#' @examples
#' \dontrun{
#' vdb_info(db)
#' vdb_info("my_docs")
#' }
vdb_info <- function(db) {
  if (is.character(db)) {
    db <- vdb_open(db)
  }

  info <- list(
    name = db$name,
    path = db$path,
    count = db$count(),
    dimension = db$dimension,
    model = db$model_name,
    tier = db$tier
  )

  cli_print("Collection Info:", "info")
  message(sprintf("  Name:      %s", info$name))
  message(sprintf("  Path:      %s", info$path))
  message(sprintf("  Documents: %d", info$count))
  message(sprintf("  Dimension: %d", info$dimension))
  message(sprintf("  Model:     %s", info$model))
  message(sprintf("  Tier:      %s", info$tier))

  invisible(info)
}

#' Collection Statistics
#'
#' @description Get detailed statistics
#' @param db Vectrix object or collection name
#' @return Named list of stats
#' @export
vdb_stats <- function(db) {
  if (is.character(db)) {
    db <- vdb_open(db)
  }

  stats <- list(
    name = db$name,
    document_count = db$count(),
    dimension = db$dimension,
    model_type = db$model_type,
    model_name = db$model_name,
    tier = db$tier,
    memory_mb = as.numeric(object.size(db)) / 1024 / 1024
  )

  cli_print("Collection Statistics:", "info")
  for (name in names(stats)) {
    val <- stats[[name]]
    if (is.numeric(val) && name == "memory_mb") {
      message(sprintf("  %-15s: %.2f MB", name, val))
    } else {
      message(sprintf("  %-15s: %s", name, val))
    }
  }

  invisible(stats)
}

# =============================================================================
# Import/Export
# =============================================================================

#' Export Collection
#'
#' @description Export collection to JSON file
#' @param db Vectrix object or collection name
#' @param path Output file path
#' @return Logical success
#' @export
#'
#' @examples
#' \dontrun{
#' vdb_export(db, "backup.json")
#' }
vdb_export <- function(db, path) {
  if (is.character(db)) {
    db <- vdb_open(db)
  }

  cli_print(sprintf("Exporting collection to '%s'...", path), "info")

  # Get all documents
  # Note: This is a simplified export - full implementation would need
  # access to internal storage
  export_data <- list(
    name = db$name,
    dimension = db$dimension,
    model = db$model_name,
    tier = db$tier,
    exported_at = Sys.time()
  )

  jsonlite::write_json(export_data, path, pretty = TRUE, auto_unbox = TRUE)
  cli_print(sprintf("Exported to '%s'", path), "success")

  invisible(TRUE)
}

#' Import from File
#'
#' @description Import documents from text file
#' @param db Vectrix object or collection name
#' @param path Input file path
#' @param separator Line separator for documents
#' @return Vectrix object
#' @export
#'
#' @examples
#' \dontrun{
#' vdb_import(db, "documents.txt")
#' }
vdb_import <- function(db, path, separator = "\n") {
  if (is.character(db)) {
    db <- vdb_open(db)
  }

  cli_print(sprintf("Importing from '%s'...", path), "info")

  if (!file.exists(path)) {
    cli_print(sprintf("File not found: %s", path), "error")
    return(invisible(db))
  }

  content <- readLines(path, warn = FALSE)

  if (separator == "\n") {
    texts <- content[nchar(trimws(content)) > 0]
  } else {
    content <- paste(content, collapse = "\n")
    texts <- strsplit(content, separator)[[1]]
    texts <- trimws(texts)
    texts <- texts[nchar(texts) > 0]
  }

  if (length(texts) == 0) {
    cli_print("No documents found in file", "warning")
    return(invisible(db))
  }

  vdb_add(db, texts)
  invisible(db)
}

# =============================================================================
# Batch Operations
# =============================================================================

#' Batch Add from Directory
#'
#' @description Add all text files from a directory
#' @param db Vectrix object or collection name
#' @param dir_path Directory path
#' @param pattern File pattern (default: "*.txt")
#' @param recursive Search subdirectories
#' @return Vectrix object
#' @export
#'
#' @examples
#' \dontrun{
#' vdb_add_dir(db, "./documents/")
#' }
vdb_add_dir <- function(db, dir_path, pattern = "\\.txt$", recursive = TRUE) {
  if (is.character(db)) {
    db <- vdb_open(db)
  }

  if (!dir.exists(dir_path)) {
    cli_print(sprintf("Directory not found: %s", dir_path), "error")
    return(invisible(db))
  }

  files <- list.files(dir_path, pattern = pattern, full.names = TRUE, recursive = recursive)

  if (length(files) == 0) {
    cli_print("No matching files found", "warning")
    return(invisible(db))
  }

  cli_print(sprintf("Found %d file(s) to import...", length(files)), "info")

  for (file in files) {
    content <- paste(readLines(file, warn = FALSE), collapse = "\n")
    if (nchar(trimws(content)) > 0) {
      db$add(content, metadata = list(source = basename(file)))
    }
  }

  cli_print(sprintf("Imported %d file(s). Total documents: %d",
                    length(files), db$count()), "success")
  invisible(db)
}

# =============================================================================
# Interactive Mode
# =============================================================================

#' Start Interactive CLI
#'
#' @description Start an interactive VectrixDB session
#' @param collection Default collection name
#' @export
#'
#' @examples
#' \dontrun{
#' vdb_interactive()
#' }
vdb_interactive <- function(collection = NULL) {
  cli_print("VectrixDB Interactive Mode", "info")
  cli_print("Commands: list, create, open, add, search, info, quit", "info")
  message("")

  db <- NULL
  if (!is.null(collection)) {
    db <- vdb_open(collection)
  }

  repeat {
    prompt <- if (!is.null(db)) sprintf("vdb[%s]> ", db$name) else "vdb> "
    cmd <- readline(prompt)

    if (cmd == "" || cmd == "quit" || cmd == "exit" || cmd == "q") {
      cli_print("Goodbye!", "info")
      break
    }

    parts <- strsplit(trimws(cmd), "\\s+")[[1]]
    action <- parts[1]
    args <- parts[-1]

    tryCatch({
      result <- switch(action,
        "list" = vdb_list(),
        "create" = { db <- vdb_create(args[1]); db },
        "open" = { db <- vdb_open(args[1]); db },
        "add" = { if (!is.null(db)) vdb_add(db, paste(args, collapse = " ")) },
        "search" = { if (!is.null(db)) vdb_search(db, paste(args, collapse = " ")) },
        "info" = { if (!is.null(db)) vdb_info(db) },
        "stats" = { if (!is.null(db)) vdb_stats(db) },
        "count" = { if (!is.null(db)) message(db$count()) },
        "clear" = { if (!is.null(db)) { db$clear(); cli_print("Cleared", "success") } },
        "help" = {
          message("Commands:")
          message("  list              - List collections")
          message("  create <name>     - Create collection")
          message("  open <name>       - Open collection")
          message("  add <text>        - Add document")
          message("  search <query>    - Search collection")
          message("  info              - Collection info")
          message("  stats             - Collection stats")
          message("  count             - Document count")
          message("  clear             - Clear collection")
          message("  quit              - Exit")
        },
        cli_print(sprintf("Unknown command: %s. Type 'help' for commands.", action), "warning")
      )
    }, error = function(e) {
      cli_print(e$message, "error")
    })
  }

  invisible(db)
}

#' Null coalescing for cli.R
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a
