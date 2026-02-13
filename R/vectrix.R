#' VectrixDB Easy API - The Simplest Vector Database
#'
#' @description
#' Zero config. Text in, results out. One line for everything.
#'
#' @examples
#' \dontrun{
#' # Create and add - ONE LINE
#' db <- Vectrix$new("my_docs")$add(c("Python is great", "Machine learning is fun"))
#'
#' # Search - ONE LINE
#' results <- db$search("programming")
#'
#' # Full power - STILL ONE LINE
#' results <- db$search("AI", mode = "ultimate")  # dense + sparse + rerank
#' }
#'
#' @name Vectrix
#' @export
Vectrix <- R6::R6Class(
  "Vectrix",
  public = list(
    #' @field name Collection name
    name = NULL,
    #' @field path Storage path
    path = NULL,
    #' @field dimension Vector dimension
    dimension = NULL,
    #' @field model_name Model identifier
    model_name = NULL,
    #' @field model_type Model type
    model_type = NULL,
    #' @field language Language setting
    language = NULL,
    #' @field tier Storage tier
    tier = NULL,

    #' @description Create or open a VectrixDB collection
    #'
    #' @param name Collection name
    #' @param path Storage path (default: ./vectrixdb_data)
    #' @param model Embedding model: "tfidf" (default), "glove-50", "glove-100",
    #'              "glove-200", "glove-300", or "word2vec"
    #' @param dimension Vector dimension (auto-detected for GloVe)
    #' @param embed_fn Custom embedding function: fn(texts) -> matrix
    #' @param model_path Path to pre-trained word vectors (GloVe .txt or word2vec .bin)
    #' @param language Language behavior: "en" (English-focused) or "ml" (multilingual/Unicode)
    #' @param tier Storage tier: "dense", "hybrid", "ultimate", or "graph"
    #' @param auto_download Automatically download GloVe vectors if needed (default: TRUE)
    #'
    #' @examples
    #' \dontrun{
    #' # Default TF-IDF embeddings (no external files needed)
    #' db <- Vectrix$new("docs")
    #'
    #' # With GloVe 100d word vectors (auto-downloads ~130MB)
    #' db <- Vectrix$new("docs", model = "glove-100")
    #'
    #' # With pre-downloaded GloVe
    #' db <- Vectrix$new("docs", model_path = "path/to/glove.6B.100d.txt")
    #'
    #' # Custom embedding function
    #' db <- Vectrix$new("docs", embed_fn = my_embed_function, dimension = 768)
    #' }
    initialize = function(name = "default",
                          path = "./vectrixdb_data",
                          model = NULL,
                          dimension = NULL,
                          embed_fn = NULL,
                          model_path = NULL,
                          language = NULL,
                          tier = "dense",
                          auto_download = TRUE) {

      self$name <- name
      self$path <- get_data_path(path)
      self$language <- normalize_language_tag(language, default = "en")
      self$tier <- tolower(tier)

      # Validate tier
      valid_tiers <- c("dense", "hybrid", "ultimate", "graph")
      if (!(self$tier %in% valid_tiers)) {
        stop(sprintf("Invalid tier '%s'. Must be one of: %s",
                     tier, paste(valid_tiers, collapse = ", ")))
      }

      # Store custom embedding function
      private$embed_fn <- embed_fn
      private$model_path <- model_path
      private$auto_download <- auto_download

      # Parse model configuration
      private$parse_model(model, dimension)

      # Initialize storage and collection
      private$init_db()
    },

    #' @description Add texts to the collection
    #'
    #' @param texts Single text or character vector of texts
    #' @param metadata Optional metadata list or list of lists
    #' @param ids Optional custom IDs
    #' @return Self for chaining
    #'
    #' @examples
    #' \dontrun{
    #' db$add(c("text 1", "text 2"))
    #' db$add("another text", metadata = list(source = "web"))
    #' }
    add = function(texts, metadata = NULL, ids = NULL) {
      # Normalize inputs
      if (is.character(texts) && length(texts) == 1) {
        texts <- list(texts)
      } else {
        texts <- as.list(texts)
      }

      n <- length(texts)

      if (is.null(metadata)) {
        metadata <- replicate(n, list(), simplify = FALSE)
      } else if (!is.list(metadata[[1]])) {
        metadata <- list(metadata)
      }

      if (is.null(ids)) {
        ids <- sapply(texts, generate_id)
      }

      # Generate embeddings
      vectors <- private$embed(unlist(texts))

      # Check if dimension changed (TF-IDF adapts to vocabulary)
      actual_dim <- ncol(vectors)
      if (actual_dim != self$dimension) {
        self$dimension <- actual_dim
        # Reinitialize collection with correct dimension
        private$collection <- Collection$new(
          name = self$name,
          dimension = self$dimension,
          metric = "cosine",
          language = self$language
        )
      }

      # Store texts for retrieval
      for (i in seq_len(n)) {
        private$texts[[ids[i]]] <- texts[[i]]
      }

      # Add to collection
      private$collection$add(
        ids = ids,
        vectors = vectors,
        metadata = metadata,
        texts = unlist(texts)
      )

      private$save_snapshot()

      invisible(self)
    },

    #' @description Update collection language behavior
    #' @param language Language behavior: "en" or "ml"
    #' @return Self for chaining
    set_language = function(language = "en") {
      next_lang <- normalize_language_tag(language, default = self$language %||% "en")
      if (identical(next_lang, self$language)) {
        return(invisible(self))
      }

      self$language <- next_lang

      if (!is.null(private$collection)) {
        private$collection$language <- self$language
      }

      if (!is.null(private$embedder)) {
        if (identical(self$model_type, "tfidf")) {
          private$embedder <- DenseEmbedder$new(
            dimension = self$dimension,
            model_type = "tfidf",
            language = self$language
          )
          private$ensure_embedder_ready()
        } else if (!is.null(private$embedder$language)) {
          private$embedder$language <- self$language
        }
      }

      if (!is.null(private$sparse_embedder)) {
        private$sparse_embedder <- SparseEmbedder$new(language = self$language)
        corpus <- unlist(private$texts, use.names = FALSE)
        corpus <- as.character(corpus)
        corpus <- corpus[nzchar(corpus)]
        if (length(corpus)) {
          try(private$sparse_embedder$fit(corpus), silent = TRUE)
        }
      }

      if (!is.null(private$reranker)) {
        private$reranker <- RerankerEmbedder$new(language = self$language)
      }

      if (!is.null(private$late_interaction)) {
        private$late_interaction <- LateInteractionEmbedder$new(language = self$language)
      }

      private$save_snapshot()
      invisible(self)
    },

    #' @description Search the collection
    #'
    #' @param query Search query text
    #' @param limit Number of results (default: 10)
    #' @param mode Search mode: "dense", "sparse", "hybrid", "ultimate"
    #' @param rerank Reranking method: NULL, "mmr", "exact", "cross-encoder"
    #' @param filter Metadata filter
    #' @param diversity Diversity parameter for MMR (0-1)
    #' @return Results object with search results
    #'
    #' @examples
    #' \dontrun{
    #' results <- db$search("python programming")
    #' results <- db$search("AI", mode = "ultimate", rerank = "mmr")
    #' print(results$top()$text)
    #' }
    search = function(query, limit = 10,
                      mode = "hybrid",
                      rerank = NULL,
                      filter = NULL,
                      diversity = 0.7) {

      start_time <- Sys.time()

      # Make sure query embedding uses collection vocabulary/model state.
      private$ensure_embedder_ready()

      # Embed query
      query_vector <- private$embed(query)[1, ]

      # Determine search strategy
      results <- switch(mode,
        "ultimate" = private$ultimate_search(query, query_vector, limit, filter, diversity),
        "dense" = private$dense_search(query_vector, limit, filter),
        "sparse" = private$sparse_search(query, limit, filter),
        "hybrid" = private$hybrid_search(query, query_vector, limit, filter),
        stop(sprintf("Unknown mode: %s. Use 'dense', 'sparse', 'hybrid', or 'ultimate'", mode))
      )

      # Apply reranking if requested
      if (!is.null(rerank) && mode != "ultimate") {
        results <- private$rerank_results(query, query_vector, results, rerank, limit, diversity)
      }

      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs")) * 1000

      # Convert to Results object
      items <- lapply(results, function(r) {
        Result$new(
          id = r$id,
          text = private$texts[[r$id]] %||% r$text %||% "",
          score = r$score,
          metadata = r$metadata %||% list()
        )
      })

      Results$new(
        items = items,
        query = query,
        mode = mode,
        time_ms = elapsed
      )
    },

    #' @description Delete documents by ID
    #' @param ids Document ID(s) to delete
    #' @return Self for chaining
    delete = function(ids) {
      if (is.character(ids) && length(ids) == 1) {
        ids <- list(ids)
      }

      private$collection$delete(ids = unlist(ids))

      for (id in ids) {
        private$texts[[id]] <- NULL
      }

      private$save_snapshot()

      invisible(self)
    },

    #' @description Clear all documents from collection
    #' @return Self for chaining
    clear = function() {
      private$collection$clear()
      private$texts <- list()
      private$save_snapshot()
      invisible(self)
    },

    #' @description Get number of documents
    #' @return Integer count
    count = function() {
      private$collection$count()
    },

    #' @description Get documents by ID
    #' @param ids Document ID(s)
    #' @return List of Result objects
    get = function(ids) {
      if (is.character(ids) && length(ids) == 1) {
        ids <- list(ids)
      }

      results <- private$collection$get(ids = unlist(ids))

      lapply(results, function(r) {
        if (is.null(r)) return(NULL)
        Result$new(
          id = r$id,
          text = private$texts[[r$id]] %||% "",
          score = 1.0,
          metadata = r$metadata %||% list()
        )
      })
    },

    #' @description Find similar documents to a given document
    #' @param id Document ID
    #' @param limit Number of results
    #' @return Results object
    similar = function(id, limit = 10) {
      # Get the document's vector
      docs <- private$collection$get(ids = id)
      if (length(docs) == 0 || is.null(docs[[1]])) {
        return(Results$new(items = list(), query = paste("similar to", id),
                           mode = "dense", time_ms = 0))
      }

      vector <- docs[[1]]$vector
      if (is.null(vector)) {
        stop(sprintf("Document %s has no vector", id))
      }

      results <- private$dense_search(vector, limit + 1, NULL)

      # Remove the query document itself
      results <- results[sapply(results, function(r) r$id != id)]
      results <- results[1:min(limit, length(results))]

      items <- lapply(results, function(r) {
        Result$new(
          id = r$id,
          text = private$texts[[r$id]] %||% "",
          score = r$score,
          metadata = r$metadata %||% list()
        )
      })

      Results$new(
        items = items,
        query = paste("similar to", id),
        mode = "dense",
        time_ms = 0
      )
    },

    #' @description Close the database connection
    close = function() {
      private$save_snapshot()
      invisible(NULL)
    },

    #' @description Print Vectrix summary
    print = function() {
      cat(sprintf("Vectrix('%s', %d docs, model='%s')\n",
                  self$name, self$count(), self$model_name))
      invisible(self)
    }
  ),

  private = list(
    collection = NULL,
    texts = NULL,
    embedder = NULL,
    sentence_embedder = NULL,
    sparse_embedder = NULL,
    reranker = NULL,
    advanced_reranker = NULL,
    late_interaction = NULL,
    embed_fn = NULL,
    model_path = NULL,
    auto_download = TRUE,

    # GloVe model dimensions
    glove_dims = list(
      "glove-50" = 50,
      "glove-100" = 100,
      "glove-200" = 200,
      "glove-300" = 300,
      "glove-twitter-25" = 25,
      "glove-twitter-50" = 50,
      "glove-twitter-100" = 100,
      "glove-twitter-200" = 200
    ),

    collection_dir = function() {
      base_path <- normalizePath(self$path, winslash = "/", mustWork = FALSE)
      if (tolower(basename(base_path)) == tolower(as.character(self$name))) {
        return(base_path)
      }
      file.path(base_path, self$name)
    },

    snapshot_path = function() {
      file.path(private$collection_dir(), "collection.db")
    },

    legacy_snapshot_path = function() {
      file.path(private$collection_dir(), "vector_index.rds")
    },

    chunks_path = function() {
      file.path(private$collection_dir(), "chunks.rds")
    },

    catalog_db_path = function() {
      file.path(self$path, "_vectrixdb.db")
    },

    sqlite_available = function() {
      requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)
    },

    write_catalog_row = function(count = NULL) {
      if (!private$sqlite_available()) {
        return(invisible(FALSE))
      }

      dir.create(self$path, recursive = TRUE, showWarnings = FALSE)

      con <- tryCatch(
        DBI::dbConnect(RSQLite::SQLite(), private$catalog_db_path()),
        error = function(e) NULL
      )
      if (is.null(con)) {
        return(invisible(FALSE))
      }
      on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

      tryCatch({
        DBI::dbExecute(
          con,
          "CREATE TABLE IF NOT EXISTS collections (
            name TEXT PRIMARY KEY,
            dimension INTEGER,
            metric TEXT,
            tier TEXT,
            language TEXT,
            tags TEXT,
            description TEXT,
            chunk_count INTEGER DEFAULT 0,
            updated_at TEXT
          )"
        )

        lang_tag <- normalize_language_tag(self$language, default = "en")
        tags <- unique(c(tolower(as.character(self$tier %||% "dense")), lang_tag))
        tags_json <- as.character(jsonlite::toJSON(as.list(tags), auto_unbox = TRUE))

        if (is.null(count)) {
          count <- tryCatch(as.integer(private$collection$count()), error = function(e) 0L)
        }
        count <- as.integer(count %||% 0L)

        DBI::dbExecute(
          con,
          "INSERT OR REPLACE INTO collections
           (name, dimension, metric, tier, language, tags, chunk_count, updated_at)
           VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
          params = list(
            as.character(self$name),
            as.integer(self$dimension %||% 0L),
            "cosine",
            as.character(self$tier %||% "dense"),
            lang_tag,
            tags_json,
            count,
            as.character(Sys.time())
          )
        )
        TRUE
      }, error = function(e) FALSE)
    },

    save_sqlite = function(ids, vectors, metadatas, texts) {
      if (!private$sqlite_available()) {
        return(FALSE)
      }

      dir.create(private$collection_dir(), recursive = TRUE, showWarnings = FALSE)

      con <- tryCatch(
        DBI::dbConnect(RSQLite::SQLite(), private$snapshot_path()),
        error = function(e) NULL
      )
      if (is.null(con)) {
        return(FALSE)
      }
      on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

      tryCatch({
        DBI::dbExecute(
          con,
          "CREATE TABLE IF NOT EXISTS collection_meta (
            key TEXT PRIMARY KEY,
            value TEXT
          )"
        )
        DBI::dbExecute(
          con,
          "CREATE TABLE IF NOT EXISTS points (
            id TEXT PRIMARY KEY,
            vector_json TEXT NOT NULL,
            metadata_json TEXT,
            text TEXT
          )"
        )

        DBI::dbExecute(con, "BEGIN TRANSACTION")
        DBI::dbExecute(con, "DELETE FROM points")
        DBI::dbExecute(con, "DELETE FROM collection_meta")

        if (length(ids) > 0) {
          vector_json <- vapply(seq_along(ids), function(i) {
            as.character(jsonlite::toJSON(as.numeric(vectors[i, ]), auto_unbox = TRUE))
          }, character(1))
          metadata_json <- vapply(seq_along(ids), function(i) {
            as.character(jsonlite::toJSON(metadatas[[i]] %||% list(), auto_unbox = TRUE, null = "null"))
          }, character(1))

          points_df <- data.frame(
            id = as.character(ids),
            vector_json = vector_json,
            metadata_json = metadata_json,
            text = as.character(texts),
            stringsAsFactors = FALSE
          )

          DBI::dbWriteTable(con, "points", points_df, append = TRUE, row.names = FALSE)
        }

        meta_df <- data.frame(
          key = c("name", "dimension", "tier", "language", "model_name", "model_type", "updated_at"),
          value = c(
            as.character(self$name),
            as.character(ncol(vectors)),
            as.character(self$tier %||% "dense"),
            as.character(self$language %||% ""),
            as.character(self$model_name %||% ""),
            as.character(self$model_type %||% ""),
            as.character(Sys.time())
          ),
          stringsAsFactors = FALSE
        )
        DBI::dbWriteTable(con, "collection_meta", meta_df, append = TRUE, row.names = FALSE)

        DBI::dbExecute(con, "COMMIT")
        TRUE
      }, error = function(e) {
        try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
        FALSE
      })
    },

    load_sqlite = function() {
      db_file <- private$snapshot_path()
      if (!file.exists(db_file) || !private$sqlite_available()) {
        return(invisible(FALSE))
      }

      con <- tryCatch(
        DBI::dbConnect(RSQLite::SQLite(), db_file),
        error = function(e) NULL
      )
      if (is.null(con)) {
        return(invisible(FALSE))
      }
      on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

      has_points <- tryCatch(DBI::dbExistsTable(con, "points"), error = function(e) FALSE)
      has_meta <- tryCatch(DBI::dbExistsTable(con, "collection_meta"), error = function(e) FALSE)
      if (!has_points) {
        return(invisible(FALSE))
      }

      meta <- list()
      if (has_meta) {
        meta_rows <- tryCatch(DBI::dbGetQuery(con, "SELECT key, value FROM collection_meta"), error = function(e) data.frame())
        if (nrow(meta_rows) > 0) {
          meta <- as.list(meta_rows$value)
          names(meta) <- meta_rows$key
        }
      }
      self$language <- normalize_language_tag(meta$language %||% self$language, default = "en")

      points <- tryCatch(
        DBI::dbGetQuery(con, "SELECT id, vector_json, metadata_json, text FROM points ORDER BY rowid"),
        error = function(e) data.frame()
      )

      if (nrow(points) == 0) {
        self$language <- normalize_language_tag(meta$language %||% self$language, default = "en")
        dim_meta <- suppressWarnings(as.integer(meta$dimension %||% NA))
        if (!is.na(dim_meta) && dim_meta > 0L && dim_meta != self$dimension) {
          self$dimension <- dim_meta
          private$collection <- Collection$new(
            name = self$name,
            dimension = self$dimension,
            metric = "cosine",
            language = self$language
          )
        }
        return(invisible(TRUE))
      }

      ids <- as.character(points$id)
      vec_list <- lapply(points$vector_json, function(v) {
        as.numeric(tryCatch(jsonlite::fromJSON(v), error = function(e) numeric(0)))
      })

      valid <- lengths(vec_list) > 0 & nzchar(ids)
      if (!any(valid)) {
        return(invisible(FALSE))
      }

      ids <- ids[valid]
      vec_list <- vec_list[valid]
      points <- points[valid, , drop = FALSE]

      vectors <- tryCatch(do.call(rbind, vec_list), error = function(e) NULL)
      if (is.null(vectors) || !is.matrix(vectors) || nrow(vectors) != length(ids)) {
        return(invisible(FALSE))
      }

      texts <- as.character(points$text %||% rep("", length(ids)))
      if (length(texts) != length(ids)) {
        texts <- rep("", length(ids))
      }

      metadatas <- lapply(points$metadata_json, function(m) {
        tryCatch(jsonlite::fromJSON(m, simplifyVector = FALSE), error = function(e) list())
      })
      if (length(metadatas) != length(ids)) {
        metadatas <- replicate(length(ids), list(), simplify = FALSE)
      }

      if (ncol(vectors) != self$dimension) {
        self$dimension <- ncol(vectors)
        private$collection <- Collection$new(
          name = self$name,
          dimension = self$dimension,
          metric = "cosine",
          language = self$language
        )
      }

      tryCatch({
        private$collection$add(
          ids = ids,
          vectors = vectors,
          metadata = metadatas,
          texts = texts
        )
        private$texts <- as.list(texts)
        names(private$texts) <- ids
        TRUE
      }, error = function(e) FALSE)
    },

    save_snapshot = function() {
      if (is.null(private$collection)) {
        return(invisible(FALSE))
      }

      coll_private <- tryCatch(
        private$collection$.__enclos_env__$private,
        error = function(e) NULL
      )
      if (is.null(coll_private)) {
        return(invisible(FALSE))
      }

      ids <- as.character(coll_private$ids %||% character(0))
      vectors <- coll_private$vectors %||% matrix(numeric(0), nrow = 0, ncol = self$dimension)
      metadata_map <- coll_private$metadata %||% list()
      texts_map <- coll_private$texts %||% list()

      if (!is.matrix(vectors)) {
        vectors <- as.matrix(vectors)
      }
      if (nrow(vectors) != length(ids)) {
        return(invisible(FALSE))
      }

      meta_rows <- lapply(ids, function(id) metadata_map[[id]] %||% list())
      text_rows <- vapply(ids, function(id) as.character(texts_map[[id]] %||% ""), character(1))

      payload <- list(
        version = 1L,
        name = self$name,
        tier = self$tier,
        language = self$language,
        model_name = self$model_name,
        model_type = self$model_type,
        dimension = ncol(vectors),
        ids = ids,
        vectors = vectors,
        metadatas = meta_rows,
        texts = text_rows,
        saved_at = as.character(Sys.time())
      )

      dir.create(private$collection_dir(), recursive = TRUE, showWarnings = FALSE)

      sqlite_ok <- private$save_sqlite(ids, vectors, meta_rows, text_rows)
      if (isTRUE(sqlite_ok)) {
        private$write_catalog_row(length(ids))
        return(invisible(TRUE))
      }

      # Fallback for environments without DBI/RSQLite.
      rds_ok <- tryCatch({
        saveRDS(payload, private$legacy_snapshot_path())
        TRUE
      }, error = function(e) FALSE)

      if (isTRUE(rds_ok)) {
        private$write_catalog_row(length(ids))
      }

      invisible(rds_ok)
    },

    load_snapshot = function() {
      # Preferred format: SQLite collection DB.
      loaded_sqlite <- private$load_sqlite()
      if (isTRUE(loaded_sqlite)) {
        private$write_catalog_row()
        return(invisible(TRUE))
      }

      # Legacy fallback: RDS snapshot.
      snapshot_file <- private$legacy_snapshot_path()
      if (!file.exists(snapshot_file)) {
        return(invisible(FALSE))
      }

      snapshot <- tryCatch(readRDS(snapshot_file), error = function(e) NULL)
      if (is.null(snapshot)) {
        return(invisible(FALSE))
      }

      self$language <- normalize_language_tag(snapshot$language %||% self$language, default = "en")

      ids <- as.character(snapshot$ids %||% character(0))
      vectors <- snapshot$vectors %||% matrix(numeric(0), nrow = 0, ncol = self$dimension)
      metadatas <- snapshot$metadatas %||% list()
      texts <- snapshot$texts %||% character(0)

      if (!length(ids)) {
        return(invisible(TRUE))
      }

      if (!is.matrix(vectors)) {
        vectors <- as.matrix(vectors)
      }
      if (nrow(vectors) != length(ids)) {
        return(invisible(FALSE))
      }
      if (length(texts) != length(ids)) {
        texts <- rep("", length(ids))
      }
      if (!is.list(metadatas) || length(metadatas) != length(ids)) {
        metadatas <- replicate(length(ids), list(), simplify = FALSE)
      }

      if (ncol(vectors) != self$dimension) {
        self$dimension <- ncol(vectors)
        private$collection <- Collection$new(
          name = self$name,
          dimension = self$dimension,
          metric = "cosine",
          language = self$language
        )
      }

      loaded_rds <- tryCatch({
        private$collection$add(
          ids = ids,
          vectors = vectors,
          metadata = metadatas,
          texts = texts
        )
        private$texts <- as.list(texts)
        names(private$texts) <- ids
        TRUE
      }, error = function(e) FALSE)

      if (isTRUE(loaded_rds)) {
        # Auto-migrate old RDS collections to SQLite when possible.
        private$save_snapshot()
        private$write_catalog_row()
      }

      loaded_rds
    },

    bootstrap_from_chunks = function() {
      chunk_file <- private$chunks_path()
      if (!file.exists(chunk_file)) {
        return(invisible(FALSE))
      }

      chunks <- tryCatch(readRDS(chunk_file), error = function(e) NULL)
      if (is.null(chunks) || !length(chunks)) {
        return(invisible(FALSE))
      }

      texts <- vapply(chunks, function(ch) as.character(ch$text %||% ""), character(1))
      ids <- vapply(chunks, function(ch) as.character(ch$id %||% generate_id(ch$text %||% "")), character(1))
      metadatas <- lapply(chunks, function(ch) ch$metadata %||% list())

      valid <- nzchar(texts) & nzchar(ids)
      if (!any(valid)) {
        return(invisible(FALSE))
      }

      texts <- texts[valid]
      ids <- ids[valid]
      metadatas <- metadatas[valid]

      vectors <- private$embed(texts)
      if (!is.matrix(vectors)) {
        vectors <- as.matrix(vectors)
      }
      if (nrow(vectors) != length(ids)) {
        return(invisible(FALSE))
      }

      if (ncol(vectors) != self$dimension) {
        self$dimension <- ncol(vectors)
        private$collection <- Collection$new(
          name = self$name,
          dimension = self$dimension,
          metric = "cosine",
          language = self$language
        )
      }

      private$collection$add(
        ids = ids,
        vectors = vectors,
        metadata = metadatas,
        texts = texts
      )
      private$texts <- as.list(texts)
      names(private$texts) <- ids
      private$save_snapshot()
      TRUE
    },

    parse_model = function(model, dimension) {
      # Custom embedding function provided
      if (!is.null(private$embed_fn)) {
        self$model_type <- "custom"
        self$model_name <- model %||% "custom"
        self$dimension <- dimension %||% 100
        return()
      }

      # Model path provided (GloVe or word2vec file)
      if (!is.null(private$model_path)) {
        ext <- tolower(tools::file_ext(private$model_path))
        if (ext == "bin") {
          self$model_type <- "word2vec"
        } else {
          self$model_type <- "glove"
        }
        self$model_name <- private$model_path
        self$dimension <- dimension %||% 100
        return()
      }

      # No model specified - use TF-IDF (default, no external files)
      if (is.null(model)) {
        self$model_type <- "tfidf"
        self$model_name <- "tfidf"
        self$dimension <- dimension %||% 100
        return()
      }

      # Handle model types
      model_lower <- tolower(model)

      # Check for GloVe model names (e.g., "glove-100", "glove-300")
      if (model_lower %in% names(private$glove_dims)) {
        self$model_type <- "glove-pretrained"
        self$model_name <- model_lower
        self$dimension <- private$glove_dims[[model_lower]]
        return()
      }

      if (model_lower %in% c("tfidf", "tf-idf")) {
        self$model_type <- "tfidf"
        self$model_name <- "tfidf"
        self$dimension <- dimension %||% 100
      } else if (model_lower == "glove") {
        # Default to glove-100
        self$model_type <- "glove-pretrained"
        self$model_name <- "glove-100"
        self$dimension <- 100
      } else if (model_lower == "word2vec") {
        self$model_type <- "word2vec"
        self$model_name <- "word2vec"
        self$dimension <- dimension %||% 100
      } else {
        # Default to TF-IDF for unknown models
        self$model_type <- "tfidf"
        self$model_name <- "tfidf"
        self$dimension <- dimension %||% 100
      }
    },

    init_db = function() {
      private$texts <- list()
      private$collection <- Collection$new(
        name = self$name,
        dimension = self$dimension,
        metric = "cosine",
        language = self$language
      )

      # Initialize embedder
      private$init_embedder()

      # Load persisted vectors (if present)
      loaded <- private$load_snapshot()
      if (!isTRUE(loaded)) {
        private$bootstrap_from_chunks()
      }

      # Keep TF-IDF embedder dimension aligned to the loaded collection.
      private$ensure_embedder_dimension()

      private$write_catalog_row()
    },

    init_embedder = function() {
      if (self$model_type == "custom") {
        # Custom function, no embedder needed
        return()
      }

      # Handle GloVe pretrained models
      if (self$model_type == "glove-pretrained") {
        tryCatch({
          # Download if needed
          if (private$auto_download) {
            message(sprintf("Initializing %s embeddings...", self$model_name))
            path <- download_word_vectors(self$model_name)
            wv <- load_word_vectors(path)
            private$sentence_embedder <- SentenceEmbedder$new(wv, use_idf = TRUE)
            private$embedder <- private$sentence_embedder
            # Initialize advanced reranker with sentence embedder
            private$advanced_reranker <- AdvancedReranker$new(
              semantic_weight = 0.4,
              bm25_weight = 0.3,
              coverage_weight = 0.2,
              position_weight = 0.1,
              sentence_embedder = private$sentence_embedder
            )
            message("GloVe embeddings ready!")
          } else {
            stop("GloVe model requires auto_download=TRUE or provide model_path")
          }
        }, error = function(e) {
          warning(sprintf("Could not load GloVe: %s. Falling back to TF-IDF.", e$message))
          self$model_type <- "tfidf"
          self$model_name <- "tfidf"
          private$embedder <- DenseEmbedder$new(
            dimension = self$dimension,
            language = self$language
          )
        })
        return()
      }

      tryCatch({
        private$embedder <- DenseEmbedder$new(
          dimension = self$dimension,
          model_path = private$model_path,
          model_type = self$model_type,
          language = self$language
        )
        # Also initialize sparse embedder for hybrid search
        private$sparse_embedder <- SparseEmbedder$new(language = self$language)
        # Initialize reranker
        private$reranker <- RerankerEmbedder$new(language = self$language)
      }, error = function(e) {
        warning(sprintf("Could not initialize embedder: %s", e$message))
      })
    },

    embed = function(texts) {
      if (is.character(texts) && length(texts) == 1) {
        texts <- c(texts)
      }

      if (self$model_type == "custom") {
        # Use custom embedding function
        result <- private$embed_fn(texts)
        if (!is.matrix(result)) {
          result <- matrix(result, nrow = length(texts))
        }
        return(result)
      }

      if (!is.null(private$embedder)) {
        result <- private$embedder$embed(texts)
        # Guard against zero-width vectors from bad/empty vocabulary states.
        if (ncol(result) == 0) {
          return(matrix(0, nrow = length(texts), ncol = self$dimension))
        }
        # Keep query/add vectors aligned to collection dimension without mutating it.
        target_dim <- as.integer(self$dimension %||% ncol(result))
        if (target_dim > 0L && ncol(result) != target_dim) {
          if (ncol(result) < target_dim) {
            pad <- matrix(0, nrow = nrow(result), ncol = target_dim - ncol(result))
            result <- cbind(result, pad)
          } else {
            result <- result[, seq_len(target_dim), drop = FALSE]
          }
        }
        return(result)
      }

      # Fallback: TF-IDF
      private$embedder <- DenseEmbedder$new(dimension = self$dimension, language = self$language)
      private$embedder$embed(texts)
    },

    ensure_embedder_dimension = function() {
      # Relevant for TF-IDF where default constructor dim can differ after reload.
      if (!identical(self$model_type, "tfidf")) {
        return(invisible(TRUE))
      }

      target_dim <- suppressWarnings(as.integer(self$dimension %||% NA))
      if (is.na(target_dim) || target_dim <= 0L) {
        return(invisible(FALSE))
      }

      current_dim <- tryCatch(
        suppressWarnings(as.integer(private$embedder$dimension %||% NA)),
        error = function(e) NA_integer_
      )
      model_type <- tryCatch(as.character(private$embedder$model_type %||% ""), error = function(e) "")
      fitted <- tryCatch(
        isTRUE(private$embedder$.__enclos_env__$private$fitted),
        error = function(e) FALSE
      )

      needs_init <- is.null(private$embedder) ||
        !identical(model_type, "tfidf") ||
        is.na(current_dim) ||
        current_dim <= 0L ||
        (!fitted && current_dim != target_dim)

      if (needs_init) {
        private$embedder <- DenseEmbedder$new(
          dimension = target_dim,
          model_type = "tfidf",
          language = self$language
        )
      }

      invisible(TRUE)
    },

    ensure_embedder_ready = function() {
      # TF-IDF must be fitted on collection texts so query vectors align with stored vectors.
      if (!identical(self$model_type, "tfidf")) {
        return(invisible(TRUE))
      }

      private$ensure_embedder_dimension()
      if (is.null(private$embedder)) {
        return(invisible(FALSE))
      }

      fitted <- tryCatch(
        isTRUE(private$embedder$.__enclos_env__$private$fitted),
        error = function(e) FALSE
      )
      if (fitted) {
        return(invisible(TRUE))
      }

      corpus <- unlist(private$texts, use.names = FALSE)
      corpus <- as.character(corpus)
      corpus <- corpus[nzchar(corpus)]
      if (!length(corpus)) {
        return(invisible(FALSE))
      }

      ok <- tryCatch({
        private$embedder$fit(corpus)
        TRUE
      }, error = function(e) FALSE)

      invisible(ok)
    },

    dense_search = function(query_vector, limit, filter) {
      results <- private$collection$search(
        query = query_vector,
        limit = limit,
        filter = filter
      )

      lapply(results$items, function(r) {
        list(id = r$id, score = r$score, metadata = r$metadata, text = r$text)
      })
    },

    sparse_search = function(query, limit, filter) {
      results <- private$collection$keyword_search(
        query_text = query,
        limit = limit,
        filter = filter
      )

      lapply(results$items, function(r) {
        list(id = r$id, score = r$score, metadata = r$metadata, text = r$text)
      })
    },

    hybrid_search = function(query, query_vector, limit, filter) {
      # hybrid = dense + sparse + rerank (cross-encoder)
      prefetch_limit <- min(limit * 5, max(private$collection$count(), 1))
      if (prefetch_limit == 0) return(list())

      # Get candidates from dense and sparse
      dense_results <- private$dense_search(query_vector, prefetch_limit, filter)
      sparse_results <- private$sparse_search(query, prefetch_limit, filter)

      # RRF Fusion
      fused <- private$rrf_fuse(dense_results, sparse_results)

      # Apply cross-encoder reranking
      if (length(fused) > 0) {
        if (is.null(private$reranker)) {
          private$reranker <- RerankerEmbedder$new(language = self$language)
        }
        doc_texts <- sapply(fused, function(r) r$text %||% private$texts[[r$id]] %||% "")
        rerank_scores <- private$reranker$score(query, doc_texts)

        # Combine RRF score with rerank score (0.6 RRF + 0.4 rerank)
        for (i in seq_along(fused)) {
          fused[[i]]$score <- 0.6 * fused[[i]]$score + 0.4 * rerank_scores[i]
        }

        # Re-sort by combined score
        order_idx <- order(vapply(fused, function(r) as.numeric(r$score)[1], numeric(1)), decreasing = TRUE)
        fused <- fused[order_idx]
      }

      fused[1:min(limit, length(fused))]
    },

    ultimate_search = function(query, query_vector, limit, filter, diversity) {
      # ultimate = dense + sparse + rerank + late interaction
      prefetch_limit <- min(limit * 10, max(private$collection$count(), 1))
      if (prefetch_limit == 0) return(list())

      # Get candidates from dense and sparse
      dense_results <- private$dense_search(query_vector, prefetch_limit, filter)
      sparse_results <- private$sparse_search(query, prefetch_limit, filter)

      # RRF Fusion
      fused <- private$rrf_fuse(dense_results, sparse_results)

      if (length(fused) == 0) return(list())

      doc_texts <- sapply(fused, function(r) r$text %||% private$texts[[r$id]] %||% "")

      # Apply cross-encoder reranking
      if (is.null(private$reranker)) {
        private$reranker <- RerankerEmbedder$new(language = self$language)
      }
      rerank_scores <- private$reranker$score(query, doc_texts)
      rerank_scores <- as.numeric(rerank_scores)
      rerank_scores[!is.finite(rerank_scores)] <- 0

      # Apply late interaction scoring (ColBERT-style MaxSim)
      if (is.null(private$late_interaction)) {
        private$late_interaction <- LateInteractionEmbedder$new(language = self$language)
      }

      query_tokens <- private$late_interaction$embed(query)[[1]]
      interact_scores <- vapply(doc_texts, function(doc_text) {
        doc_tokens <- private$late_interaction$embed(doc_text)[[1]]
        private$late_interaction$score(query_tokens, doc_tokens)
      }, numeric(1))
      interact_scores[!is.finite(interact_scores)] <- 0

      # Normalize interact scores
      max_interact <- suppressWarnings(max(interact_scores, na.rm = TRUE))
      if (isTRUE(is.finite(max_interact)) && isTRUE(max_interact > 0)) {
        interact_scores <- interact_scores / max_interact
      }

      # Combine: RRF (0.35) + rerank (0.35) + interact (0.30)
      for (i in seq_along(fused)) {
        fused[[i]]$score <- 0.35 * fused[[i]]$score +
                           0.35 * rerank_scores[i] +
                           0.30 * interact_scores[i]
      }

      # Re-sort by combined score
      order_idx <- order(vapply(fused, function(r) as.numeric(r$score)[1], numeric(1)), decreasing = TRUE)
      fused <- fused[order_idx]

      fused[1:min(limit, length(fused))]
    },

    rrf_fuse = function(dense_results, sparse_results) {
      # Reciprocal Rank Fusion
      rrf_k <- 60
      scores <- list()

      # Add dense results
      for (rank in seq_along(dense_results)) {
        r <- dense_results[[rank]]
        if (is.null(scores[[r$id]])) {
          scores[[r$id]] <- list(rrf_dense = 0, rrf_sparse = 0,
                                  metadata = r$metadata, text = r$text)
        }
        scores[[r$id]]$rrf_dense <- 1.0 / (rrf_k + rank)
      }

      # Add sparse results
      for (rank in seq_along(sparse_results)) {
        r <- sparse_results[[rank]]
        if (is.null(scores[[r$id]])) {
          scores[[r$id]] <- list(rrf_dense = 0, rrf_sparse = 0,
                                  metadata = r$metadata, text = r$text)
        }
        scores[[r$id]]$rrf_sparse <- 1.0 / (rrf_k + rank)
      }

      # Combine scores with intersection boost
      for (id in names(scores)) {
        rrf_dense <- scores[[id]]$rrf_dense
        rrf_sparse <- scores[[id]]$rrf_sparse
        combined <- 0.5 * rrf_dense + 0.5 * rrf_sparse

        if (isTRUE(rrf_dense > 0) && isTRUE(rrf_sparse > 0)) {
          combined <- combined * 1.15  # Intersection boost
        }
        scores[[id]]$combined <- combined
      }

      # Sort and return as list
      sorted_ids <- names(scores)[order(vapply(scores, function(s) as.numeric(s$combined)[1], numeric(1)), decreasing = TRUE)]

      lapply(sorted_ids, function(id) {
        list(
          id = id,
          score = scores[[id]]$combined,
          metadata = scores[[id]]$metadata,
          text = scores[[id]]$text %||% private$texts[[id]] %||% ""
        )
      })
    },

    rerank_results = function(query, query_vector, results, method, limit, diversity) {
      if (length(results) == 0) return(results)

      if (method == "mmr") {
        # Get vectors for MMR
        doc_ids <- sapply(results, function(r) r$id)
        scores <- sapply(results, function(r) r$score)

        # Get document vectors from collection
        docs <- private$collection$get(ids = doc_ids)
        doc_vectors <- do.call(rbind, lapply(docs, function(d) d$vector))

        if (is.null(doc_vectors) || nrow(doc_vectors) == 0) {
          return(results[1:min(limit, length(results))])
        }

        # Use MMRReranker if available
        mmr_reranker <- MMRReranker$new(lambda = diversity)
        reranked <- mmr_reranker$rerank(query_vector, doc_vectors, doc_ids, scores, limit)

        lapply(seq_len(nrow(reranked)), function(i) {
          id <- reranked$id[i]
          original <- results[sapply(results, function(r) r$id == id)][[1]]
          original$score <- reranked$score[i]
          original
        })
      } else if (method == "advanced" || method == "learned") {
        # Use AdvancedReranker with learned weights
        if (is.null(private$advanced_reranker)) {
          private$advanced_reranker <- AdvancedReranker$new(
            sentence_embedder = private$sentence_embedder
          )
        }

        # Get document vectors if available
        doc_ids <- sapply(results, function(r) r$id)
        docs <- private$collection$get(ids = doc_ids)
        doc_vectors <- NULL
        if (length(docs) > 0 && !is.null(docs[[1]]$vector)) {
          doc_vectors <- do.call(rbind, lapply(docs, function(d) d$vector))
        }

        reranked <- private$advanced_reranker$rerank(
          query = query,
          query_vector = query_vector,
          results = results,
          doc_vectors = doc_vectors,
          limit = limit
        )
        reranked
      } else if (method == "cross-encoder") {
        # Use reranker if available
        if (is.null(private$reranker)) {
          tryCatch({
            private$reranker <- RerankerEmbedder$new(language = self$language)
          }, error = function(e) {
            warning("Reranker not available, returning original results")
            return(results[1:min(limit, length(results))])
          })
        }

        doc_texts <- sapply(results, function(r) r$text %||% private$texts[[r$id]] %||% "")
        scores <- private$reranker$score(query, doc_texts)

        order_idx <- order(scores, decreasing = TRUE)[1:min(limit, length(scores))]

        lapply(order_idx, function(i) {
          r <- results[[i]]
          r$score <- scores[i]
          r
        })
      } else {
        # exact reranking - recompute similarities
        results[1:min(limit, length(results))]
      }
    }
  )
)

#' Create a new Vectrix collection
#'
#' @param name Collection name
#' @param ... Additional arguments passed to Vectrix$new()
#' @return Vectrix object
#' @export
vectrix_create <- function(name = "default", ...) {
  Vectrix$new(name, ...)
}

#' Open an existing Vectrix collection
#'
#' @param name Collection name
#' @param path Storage path
#' @return Vectrix object
#' @export
vectrix_open <- function(name = "default", path = "./vectrixdb_data") {
  Vectrix$new(name, path = path)
}

#' Quick search - Index texts and search immediately
#'
#' @param texts Character vector of texts to index
#' @param query Search query
#' @param limit Number of results
#' @return Results object
#' @export
#'
#' @examples
#' \dontrun{
#' results <- quick_search(
#'   texts = c("Python is great", "Java is verbose", "Rust is fast"),
#'   query = "programming language"
#' )
#' print(results$top()$text)
#' }
quick_search <- function(texts, query, limit = 5) {
  db <- Vectrix$new("_quick_search")
  db$clear()
  db$add(texts)
  db$search(query, limit = limit)
}
