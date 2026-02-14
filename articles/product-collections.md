# Product Collections

## Overview

This article documents the **product collections** workflow using the
project script:

`product collections/create_collection.R`

## Prepare Product Docs

Store source documents in a docs folder such as:

``` text
product collections/
  products/
    docs/
      bmo_credit_cards.md
```

The script chunks markdown by headings, so section structure matters.

## Build Collection

Run the same command used in the project workflow:

``` r
Rscript "product collections/create_collection.R" --clear
```

This builds `bmo_products` in `local_blob/vector database`.

## Script Code: Configuration

The current configuration in `create_collection.R`:

``` r
SCRIPT_DIR <- get_script_dir()
DOCS_PATH <- file.path(SCRIPT_DIR, "products", "docs")
BASE_DIR <- dirname(SCRIPT_DIR)

OUTPUT_PATHS <- c(
  #file.path(BASE_DIR, "05_agent registry", "backend", "data", "products"),
  file.path(BASE_DIR, "local_blob", "vector database")
)

COLLECTION_NAME <- "bmo_products"
COLLECTION_TAGS <- c("graph", "EN")
COLLECTION_TIER <- "graph"
COLLECTION_LANGUAGE <- "en"
VECTOR_DIMENSION <- 384

CHUNK_SIZE <- 800
CHUNK_OVERLAP <- 100
```

## Script Code: Markdown Chunking

Chunking logic excerpt from `create_collection.R`:

``` r
chunk_markdown <- function(content, doc_title, chunk_size = CHUNK_SIZE, overlap = CHUNK_OVERLAP) {
  chunks <- list()
  sections <- strsplit(content, "\n(?=## )", perl = TRUE)[[1]]

  for (section in sections) {
    section <- trimws(section)
    if (nchar(section) == 0) next

    lines <- strsplit(section, "\n")[[1]]
    first_line <- lines[1]

    if (startsWith(first_line, "## ")) {
      section_title <- gsub("^## ", "", first_line)
    } else if (startsWith(first_line, "# ")) {
      section_title <- gsub("^# ", "", first_line)
    } else {
      section_title <- "Overview"
    }

    if (nchar(section) <= chunk_size) {
      chunk_text <- sprintf("Document: %s\nSection: %s\n\n%s", doc_title, section_title, section)
      chunks[[length(chunks) + 1]] <- list(text = chunk_text, section = section_title)
      next
    }

    subsections <- strsplit(section, "\n(?=### )", perl = TRUE)[[1]]
    for (subsection in subsections) {
      subsection <- trimws(subsection)
      if (nchar(subsection) == 0) next
      # (remaining paragraph split + overlap logic unchanged)
    }
  }

  chunks
}
```

## Script Code: Collection Creation

Collection write path and insert logic excerpt:

``` r
create_collection_at_path <- function(chunks, data_path, clear = FALSE) {
  if (!dir.exists(data_path)) {
    dir.create(data_path, recursive = TRUE)
  }

  db <- getExportedValue("VectrixDB", "Vectrix")$new(
    name = COLLECTION_NAME,
    path = data_path,
    dimension = VECTOR_DIMENSION,
    tier = COLLECTION_TIER,
    language = COLLECTION_LANGUAGE
  )

  existing_count <- db$count()
  if (clear && existing_count > 0) {
    db$clear()
  }

  texts <- sapply(chunks, function(c) c$text)
  metadata <- lapply(chunks, function(c) c$metadata)
  ids <- sapply(chunks, function(c) c$id)

  db$add(texts = texts, metadata = metadata, ids = ids)
  db
}
```

## Script Code: Entrypoint

Entrypoint excerpt:

``` r
main <- function(clear = FALSE, update_tags = FALSE) {
  chunks <- read_and_chunk_documents(DOCS_PATH)
  databases <- create_collection(chunks, clear = clear)

  if (length(databases) > 0) {
    test_search(databases[[1]])
  }

  invisible(databases)
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  clear <- "--clear" %in% args
  update_tags <- "--update-tags" %in% args
  main(clear = clear, update_tags = update_tags)
}
```

## Validate Search

Run a quick smoke query set after ingestion:

``` r
db <- VectrixDB::Vectrix$new(name = "bmo_products", path = "local_blob/vector database")
db$search("best cashback on groceries", mode = "hybrid", limit = 3)
```

If these queries return relevant chunks, collection creation is healthy.
