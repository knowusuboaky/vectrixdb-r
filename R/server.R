#' VectrixDB Server Functions
#'
#' @description REST API and dashboard server for VectrixDB
#'
#' @name server
NULL

#' Start VectrixDB server
#'
#' @description Launch a REST API server with optional dashboard
#'
#' @param path Database path
#' @param host Host address (default: "127.0.0.1")
#' @param port Port number (default: 7377)
#' @param api_key Optional API key for authentication
#' @param dashboard Enable dashboard (default: TRUE)
#' @param launch.browser Open dashboard/docs URL in browser (default: FALSE)
#'
#' @return Invisible NULL (server runs until stopped)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' vectrix_serve(path = "./my_data", port = 7377)
#' }
vectrix_serve <- function(path = "./vectrixdb_data",
                           host = "127.0.0.1",
                           port = 7377,
                           api_key = NULL,
                           dashboard = TRUE,
                           launch.browser = FALSE) {

  if (!requireNamespace("plumber", quietly = TRUE)) {
    stop("plumber package required. Install with: install.packages('plumber')")
  }

  `%||%` <- function(a, b) if (is.null(a)) b else a
  scalar <- function(x) jsonlite::unbox(x)

  data_path <- get_data_path(path)
  auth_enabled <- !is.null(api_key) && nzchar(api_key)
  collection_store <- new.env(parent = emptyenv())
  graph_store <- new.env(parent = emptyenv())

  is_collection_store <- function(dir_path) {
    file.exists(file.path(dir_path, "collection.db")) ||
      file.exists(file.path(dir_path, "vector_index.rds")) ||
      file.exists(file.path(dir_path, "chunks.rds"))
  }

  collection_path_for <- function(name) {
    base <- normalizePath(data_path, winslash = "/", mustWork = FALSE)
    if (tolower(basename(base)) == tolower(as.character(name)) && is_collection_store(base)) {
      return(base)
    }
    file.path(base, name)
  }

  parse_body <- function(req) {
    if (is.list(req$body) && length(req$body) > 0) {
      return(req$body)
    }

    raw_body <- req$postBody %||% req$body %||% ""
    if (is.raw(raw_body)) {
      raw_body <- rawToChar(raw_body)
    }

    if (!is.character(raw_body) || !nzchar(raw_body)) {
      return(list())
    }

    parsed <- tryCatch(
      jsonlite::fromJSON(raw_body, simplifyVector = FALSE),
      error = function(e) NULL
    )

    parsed %||% list()
  }

  as_int <- function(x, default = 0L) {
    val <- suppressWarnings(as.integer(x))
    if (is.na(val)) default else val
  }

  get_api_key <- function(req) {
    key <- req$HTTP_API_KEY %||% req$HTTP_X_API_KEY
    if (!is.null(key) && nzchar(key)) {
      return(key)
    }

    auth <- req$HTTP_AUTHORIZATION
    if (is.character(auth) && grepl("^Bearer\\s+", auth, ignore.case = TRUE)) {
      return(sub("^Bearer\\s+", "", auth, ignore.case = TRUE))
    }

    NULL
  }

  require_auth <- function(req, res, write = FALSE) {
    if (!auth_enabled || !write) {
      return(TRUE)
    }

    key <- get_api_key(req)
    if (is.null(key) || !identical(key, api_key)) {
      res$status <- 401
      return(FALSE)
    }

    TRUE
  }

  list_collection_names <- function() {
    base <- normalizePath(data_path, winslash = "/", mustWork = FALSE)
    disk <- if (dir.exists(base)) {
      children <- list.dirs(base, full.names = FALSE, recursive = FALSE)
      if (is_collection_store(base)) {
        unique(c(children, basename(base)))
      } else {
        children
      }
    } else {
      character(0)
    }
    mem <- ls(collection_store, all.names = TRUE)
    unique(c(disk, mem))
  }

  resolve_tier <- function(body) {
    valid <- c("dense", "hybrid", "ultimate", "graph")
    candidates <- c(body$tier %||% character(0), unlist(body$tags %||% list(), use.names = FALSE))
    candidates <- tolower(as.character(candidates))
    matched <- candidates[candidates %in% valid]
    if (length(matched)) matched[[1]] else "hybrid"
  }

  normalize_tags <- function(tags = character(0), tier = NULL, language = NULL) {
    tag_values <- tolower(as.character(unlist(tags %||% list(), use.names = FALSE)))
    tag_values <- tag_values[!is.na(tag_values) & nzchar(tag_values)]
    tag_values[tag_values == "multi"] <- "ml"

    if (!is.null(tier) && nzchar(tier)) {
      tag_values <- c(tolower(as.character(tier)), tag_values)
    }

    if (!is.null(language) && nzchar(language)) {
      lang_tag <- tolower(as.character(language))
      lang_tag <- if (lang_tag %in% c("ml", "multi")) {
        "ml"
      } else if (lang_tag %in% c("en", "english")) {
        "en"
      } else {
        ""
      }
      if (nzchar(lang_tag)) {
        tag_values <- c(tag_values, lang_tag)
      }
    }

    if (!any(tag_values %in% c("en", "ml"))) {
      tag_values <- c(tag_values, "en")
    }

    unique(tag_values)
  }

  read_collection_meta <- function(name) {
    meta_path <- file.path(collection_path_for(name), "collection_meta.json")
    if (!file.exists(meta_path)) {
      return(list())
    }

    tryCatch(
      jsonlite::fromJSON(meta_path, simplifyVector = FALSE),
      error = function(e) list()
    )
  }

  write_collection_meta <- function(name, metadata) {
    meta_path <- file.path(collection_path_for(name), "collection_meta.json")
    tryCatch({
      dir.create(dirname(meta_path), recursive = TRUE, showWarnings = FALSE)
      jsonlite::write_json(metadata, meta_path, auto_unbox = TRUE, pretty = TRUE)
      TRUE
    }, error = function(e) FALSE)
  }

  get_vectrix <- function(name, create = FALSE, dimension = 384L, tier = "hybrid", language = NULL) {
    if (!is.character(name) || !nzchar(name)) {
      stop("Collection name is required")
    }

    if (!create && !(name %in% list_collection_names())) {
      stop(sprintf("Collection '%s' not found", name))
    }

    language_tag <- as.character(language %||% "")
    if (!nzchar(language_tag) && !create) {
      metadata <- read_collection_meta(name)
      language_tag <- as.character(metadata$language %||% "")
    }
    language_tag <- if (tolower(language_tag) %in% c("ml", "multi", "multilingual")) "ml" else "en"

    if (exists(name, envir = collection_store, inherits = FALSE)) {
      obj <- get(name, envir = collection_store, inherits = FALSE)
      if (is.function(obj$set_language)) {
        try(obj$set_language(language_tag), silent = TRUE)
      }
      return(obj)
    }

    obj <- if (create) {
      Vectrix$new(
        name = name,
        path = data_path,
        dimension = dimension,
        tier = tier,
        language = language_tag
      )
    } else {
      Vectrix$new(
        name = name,
        path = data_path,
        tier = tier,
        language = language_tag
      )
    }

    assign(name, obj, envir = collection_store)
    obj
  }

  get_collection_ids <- function(collection) {
    ids <- tryCatch(
      collection$.__enclos_env__$private$collection$.__enclos_env__$private$ids,
      error = function(e) character(0)
    )
    as.character(ids %||% character(0))
  }

  get_collection_points <- function(collection, ids) {
    tryCatch(
      collection$.__enclos_env__$private$collection$get(ids = ids),
      error = function(e) list()
    )
  }

  collection_info <- function(name) {
    metadata <- read_collection_meta(name)
    meta_tier <- tolower(as.character(metadata$tier %||% ""))
    if (!(meta_tier %in% c("dense", "hybrid", "ultimate", "graph"))) {
      meta_tier <- ""
    }

    obj <- tryCatch(
      get_vectrix(name, create = FALSE, tier = if (nzchar(meta_tier)) meta_tier else "hybrid"),
      error = function(e) NULL
    )

    if (is.null(obj)) {
      tier <- if (nzchar(meta_tier)) meta_tier else "hybrid"
      tags <- normalize_tags(
        tags = metadata$tags %||% character(0),
        tier = tier,
        language = as.character(metadata$language %||% "")
      )

      return(list(
        name = scalar(name),
        count = scalar(as_int(metadata$chunk_count %||% 0L, 0L)),
        dimension = scalar(as_int(metadata$dimension %||% 384L, 384L)),
        metric = scalar(as.character(metadata$metric %||% "cosine")),
        size_bytes = scalar(0L),
        tags = tags,
        tier = scalar(tier)
      ))
    }

    tier <- tolower(as.character(metadata$tier %||% obj$tier %||% "hybrid"))
    if (!(tier %in% c("dense", "hybrid", "ultimate", "graph"))) {
      tier <- "hybrid"
    }

    tags <- normalize_tags(
      tags = metadata$tags %||% character(0),
      tier = tier,
      language = as.character(metadata$language %||% obj$language %||% "")
    )

    list(
      name = scalar(obj$name),
      count = scalar(as_int(obj$count(), 0L)),
      dimension = scalar(as_int(obj$dimension %||% metadata$dimension %||% 384L, 384L)),
      metric = scalar(as.character(metadata$metric %||% "cosine")),
      size_bytes = scalar(0L),
      tags = tags,
      tier = scalar(tier)
    )
  }

  graph_snapshot_path <- function(name) {
    file.path(data_path, name, "graphrag", "graph_snapshot.rds")
  }

  save_graph_snapshot <- function(name, payload) {
    path <- graph_snapshot_path(name)
    tryCatch({
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      saveRDS(payload, path)
      TRUE
    }, error = function(e) FALSE)
  }

  load_graph_snapshot <- function(name) {
    path <- graph_snapshot_path(name)
    if (!file.exists(path)) {
      return(NULL)
    }
    tryCatch(readRDS(path), error = function(e) NULL)
  }

  graph_node_from_entity <- function(entity) {
    props <- entity$metadata %||% list()
    description <- as.character(entity$description %||% "")
    if (nzchar(description)) {
      props$description <- description
    }

    list(data = list(
      id = scalar(as.character(entity$id)),
      label = scalar(as.character(entity$name %||% entity$id)),
      type = scalar(as.character(entity$type %||% "OTHER")),
      properties = props
    ))
  }

  graph_edge_from_relationship <- function(rel) {
    props <- rel$metadata %||% list()
    description <- as.character(rel$description %||% "")
    if (nzchar(description)) {
      props$description <- description
    }
    props$weight <- as.numeric(rel$weight %||% 1.0)

    rel_type <- as.character(rel$type %||% "RELATED_TO")

    list(data = list(
      id = scalar(as.character(rel$id)),
      source = scalar(as.character(rel$source_id)),
      target = scalar(as.character(rel$target_id)),
      label = scalar(rel_type),
      type = scalar(rel_type),
      properties = props
    ))
  }

  build_graph_payload <- function(pipeline, limit = 500L) {
    all_entities <- unname(pipeline$graph$get_all_entities() %||% list())
    all_relationships <- unname(pipeline$graph$get_all_relationships() %||% list())

    entity_limit <- max(1L, as_int(limit, 500L))
    entities <- if (length(all_entities) > entity_limit) {
      all_entities[seq_len(entity_limit)]
    } else {
      all_entities
    }

    nodes <- lapply(entities, graph_node_from_entity)
    node_ids <- vapply(nodes, function(n) as.character(n$data$id %||% ""), character(1))

    relationships <- Filter(function(rel) {
      sid <- as.character(rel$source_id %||% "")
      tid <- as.character(rel$target_id %||% "")
      sid %in% node_ids && tid %in% node_ids
    }, all_relationships)

    edge_limit <- max(1L, entity_limit * 4L)
    if (length(relationships) > edge_limit) {
      relationships <- relationships[seq_len(edge_limit)]
    }

    edges <- lapply(relationships, graph_edge_from_relationship)
    stats <- tryCatch(pipeline$stats(), error = function(e) list())
    extractor <- tryCatch(
      tolower(as.character(pipeline$config$extractor %||% "regex")),
      error = function(e) "regex"
    )

    list(
      nodes = nodes,
      edges = edges,
      stats = list(
        entity_count = scalar(as_int(stats$entity_count %||% length(all_entities), length(all_entities))),
        relationship_count = scalar(as_int(stats$relationship_count %||% length(all_relationships), length(all_relationships))),
        community_count = scalar(as_int(stats$community_count %||% 0L, 0L)),
        extractor = scalar(extractor)
      )
    )
  }

  extract_collection_texts <- function(collection) {
    ids <- get_collection_ids(collection)
    if (!length(ids)) {
      return(list(texts = character(0), ids = character(0)))
    }

    points <- get_collection_points(collection, ids = ids)
    if (!length(points)) {
      return(list(texts = character(0), ids = character(0)))
    }

    texts <- character(0)
    doc_ids <- character(0)

    for (i in seq_along(points)) {
      point <- points[[i]]
      if (is.null(point)) next

      text <- as.character(point$text %||% point$metadata$text %||% "")
      if (!nzchar(text)) next

      point_id <- as.character(point$id %||% ids[[i]] %||% paste0("doc_", i))
      texts <- c(texts, text)
      doc_ids <- c(doc_ids, point_id)
    }

    list(texts = texts, ids = doc_ids)
  }

  build_graph_pipeline <- function(name, force_rebuild = FALSE) {
    if (!force_rebuild && exists(name, envir = graph_store, inherits = FALSE)) {
      return(get(name, envir = graph_store, inherits = FALSE))
    }

    collection <- tryCatch(get_vectrix(name, create = FALSE), error = function(e) NULL)
    if (is.null(collection)) {
      return(NULL)
    }

    docs <- extract_collection_texts(collection)
    if (!length(docs$texts)) {
      return(NULL)
    }

    config <- tryCatch(
      create_default_graphrag_config(search_type = "hybrid"),
      error = function(e) GraphRAGConfig$new(enabled = TRUE, extractor = "regex", search_type = "hybrid")
    )
    pipeline <- create_pipeline(config)
    pipeline$process(docs$texts, document_ids = docs$ids)

    assign(name, pipeline, envir = graph_store)
    save_graph_snapshot(name, build_graph_payload(pipeline, limit = 500L))
    pipeline
  }

  get_graph_payload <- function(name, limit = 500L) {
    if (exists(name, envir = graph_store, inherits = FALSE)) {
      pipeline <- get(name, envir = graph_store, inherits = FALSE)
      return(build_graph_payload(pipeline, limit = limit))
    }

    snapshot <- load_graph_snapshot(name)
    if (!is.null(snapshot)) {
      nodes <- snapshot$nodes %||% list()
      edges <- snapshot$edges %||% list()
      stats <- snapshot$stats %||% list()

      entity_limit <- max(1L, as_int(limit, 500L))
      if (length(nodes) > entity_limit) {
        nodes <- nodes[seq_len(entity_limit)]
      }

      node_ids <- vapply(nodes, function(n) as.character(n$data$id %||% ""), character(1))
      edges <- Filter(function(edge) {
        sid <- as.character(edge$data$source %||% "")
        tid <- as.character(edge$data$target %||% "")
        sid %in% node_ids && tid %in% node_ids
      }, edges)

      return(list(nodes = nodes, edges = edges, stats = stats))
    }

    NULL
  }

  extract_text_payload <- function(body) {
    texts <- character(0)
    metadatas <- list()
    ids <- NULL

    if (!is.null(body$texts)) {
      texts <- as.character(unlist(body$texts, use.names = FALSE))
      metadatas <- body$metadatas %||% body$metadata %||% replicate(length(texts), list(), simplify = FALSE)
      ids <- body$ids %||% NULL
    } else if (!is.null(body$text)) {
      texts <- as.character(body$text)
      metadatas <- list(body$metadata %||% list())
      ids <- body$id %||% NULL
    } else if (!is.null(body$documents)) {
      docs <- if (is.list(body$documents)) body$documents else as.list(body$documents)
      for (doc in docs) {
        if (is.list(doc) && !is.null(doc$text)) {
          texts <- c(texts, as.character(doc$text))
          metadatas[[length(metadatas) + 1L]] <- doc$metadata %||% list()
          ids <- c(ids, as.character(doc$id %||% NA_character_))
        } else {
          texts <- c(texts, as.character(doc))
          metadatas[[length(metadatas) + 1L]] <- list()
          ids <- c(ids, NA_character_)
        }
      }
    } else if (!is.null(body$points)) {
      points <- if (is.list(body$points)) body$points else as.list(body$points)
      for (point in points) {
        payload <- point$payload %||% list()
        text <- point$text %||% payload$text %||% ""
        texts <- c(texts, as.character(text))
        meta <- payload
        meta$text <- NULL
        metadatas[[length(metadatas) + 1L]] <- meta
        ids <- c(ids, as.character(point$id %||% NA_character_))
      }
    }

    if (!length(metadatas)) {
      metadatas <- replicate(length(texts), list(), simplify = FALSE)
    }

    if (!is.list(metadatas) || (length(metadatas) > 0 && !is.list(metadatas[[1]]))) {
      metadatas <- replicate(length(texts), metadatas, simplify = FALSE)
    }

    if (length(metadatas) != length(texts)) {
      metadatas <- replicate(length(texts), list(), simplify = FALSE)
    }

    if (is.null(ids)) {
      ids <- rep(NA_character_, length(texts))
    } else {
      ids <- as.character(unlist(ids, use.names = FALSE))
      if (length(ids) != length(texts)) {
        ids <- rep(NA_character_, length(texts))
      }
    }

    list(texts = texts, metadatas = metadatas, ids = ids)
  }

  make_ids <- function(n) {
    stamp <- as_int(as.numeric(Sys.time()) * 1000, as.integer(Sys.time()))
    sprintf("pt_%d_%04d", stamp, seq_len(n))
  }

  search_handler <- function(name, req, res, mode = "dense") {
    body <- parse_body(req)
    query <- as.character(body$query_text %||% body$query %||% body$text %||% "")
    if (!nzchar(query)) {
      res$status <- 400
      return(list(detail = "Query text is required"))
    }

    requested_language <- as.character(body$language %||% "")
    collection <- tryCatch(
      get_vectrix(name, create = FALSE, language = requested_language),
      error = function(e) NULL
    )
    if (is.null(collection)) {
      res$status <- 404
      return(list(detail = sprintf("Collection '%s' not found", name)))
    }

    limit <- as_int(body$limit %||% 10L, 10L)
    limit <- max(1L, min(limit, 100L))

    actual_mode <- mode
    if (!is.null(body$late_interaction) && isTRUE(body$late_interaction)) {
      actual_mode <- "ultimate"
    } else if (!is.null(body$rerank) && isTRUE(body$rerank)) {
      actual_mode <- "hybrid"
    }

    results <- tryCatch(
      collection$search(query = query, limit = limit, mode = actual_mode),
      error = function(e) {
        res$status <<- 400
        return(list(error = e$message))
      }
    )

    if (is.list(results) && !is.null(results$error)) {
      return(list(detail = results$error))
    }

    items <- lapply(results$items, function(item) {
      payload <- item$metadata %||% list()
      payload$text <- item$text
      list(
        id = scalar(item$id),
        score = scalar(as.numeric(item$score)),
        text = scalar(item$text),
        metadata = item$metadata %||% list(),
        payload = payload
      )
    })

    list(
      ok = scalar(TRUE),
      data = list(
        results = items,
        search_mode = scalar(actual_mode),
        query_time_ms = scalar(results$time_ms)
      ),
      results = items,
      search_mode = scalar(actual_mode),
      time_ms = scalar(results$time_ms)
    )
  }

  add_handler <- function(name, req, res) {
    if (!require_auth(req, res, write = TRUE)) {
      return(list(detail = "Invalid API key"))
    }

    collection <- tryCatch(get_vectrix(name, create = FALSE), error = function(e) NULL)
    if (is.null(collection)) {
      res$status <- 404
      return(list(detail = sprintf("Collection '%s' not found", name)))
    }

    body <- parse_body(req)
    requested_language <- as.character(body$language %||% "")
    if (nzchar(requested_language) && is.function(collection$set_language)) {
      try(collection$set_language(requested_language), silent = TRUE)
    }
    payload <- extract_text_payload(body)

    if (!length(payload$texts)) {
      res$status <- 400
      return(list(detail = "No texts provided"))
    }

    ids <- payload$ids
    missing_ids <- is.na(ids) | !nzchar(ids)
    if (any(missing_ids)) {
      ids[missing_ids] <- make_ids(sum(missing_ids))
    }

    added <- tryCatch({
      collection$add(
        texts = payload$texts,
        metadata = payload$metadatas,
        ids = ids
      )
      TRUE
    }, error = function(e) {
      res$status <<- 400
      return(list(error = e$message))
    })

    if (is.list(added) && !is.null(added$error)) {
      return(list(detail = added$error))
    }

    list(
      ok = scalar(TRUE),
      data = list(collection = scalar(name), added = scalar(length(ids)), ids = ids),
      collection = scalar(name),
      added = scalar(length(ids)),
      message = scalar("Points added")
    )
  }

  create_collection_handler <- function(req, res, api_shape = FALSE) {
    if (!require_auth(req, res, write = TRUE)) {
      return(list(detail = "Invalid API key"))
    }

    body <- parse_body(req)
    name <- as.character(body$name %||% "")
    if (!nzchar(name)) {
      res$status <- 400
      return(list(detail = "Collection name is required"))
    }

    if (name %in% list_collection_names()) {
      res$status <- 409
      return(list(detail = sprintf("Collection '%s' already exists", name)))
    }

    dimension <- as_int(body$dimension %||% 384L, 384L)
    metric <- tolower(as.character(body$metric %||% "cosine"))
    if (!(metric %in% c("cosine", "euclidean", "dot"))) {
      metric <- "cosine"
    }
    tier <- resolve_tier(body)
    requested_tags <- body$tags %||% character(0)

    requested_language <- as.character(body$language %||% "")
    if (!nzchar(requested_language)) {
      tag_probe <- tolower(as.character(unlist(requested_tags, use.names = FALSE)))
      if (any(tag_probe %in% c("ml", "multi"))) {
        requested_language <- "ml"
      } else if (any(tag_probe %in% c("en", "english"))) {
        requested_language <- "en"
      }
    }
    tags <- normalize_tags(requested_tags, tier = tier, language = requested_language)
    language_tag <- if (any(tags == "ml")) "ml" else "en"

    created <- tryCatch({
      get_vectrix(
        name,
        create = TRUE,
        dimension = dimension,
        tier = tier,
        language = language_tag
      )
      TRUE
    }, error = function(e) {
      res$status <<- 400
      return(list(error = e$message))
    })

    if (is.list(created) && !is.null(created$error)) {
      return(list(detail = created$error))
    }

    write_collection_meta(name, list(
      name = name,
      tier = tier,
      language = language_tag,
      tags = tags,
      dimension = dimension,
      metric = metric,
      updated_at = as.character(Sys.time())
    ))

    info <- collection_info(name)
    if (api_shape) {
      return(list(ok = scalar(TRUE), data = info, message = scalar("Collection created")))
    }

    list(name = scalar(name), dimension = scalar(dimension), metric = scalar(metric))
  }

  delete_collection_handler <- function(name, req, res, api_shape = FALSE) {
    if (!require_auth(req, res, write = TRUE)) {
      return(list(detail = "Invalid API key"))
    }

    if (!(name %in% list_collection_names())) {
      res$status <- 404
      return(list(detail = sprintf("Collection '%s' does not exist", name)))
    }

    if (exists(name, envir = collection_store, inherits = FALSE)) {
      rm(list = name, envir = collection_store, inherits = FALSE)
    }

    target_dir <- collection_path_for(name)
    if (dir.exists(target_dir)) {
      unlink(target_dir, recursive = TRUE, force = TRUE)
    }

    if (api_shape) {
      return(list(ok = scalar(TRUE), message = scalar("Collection deleted"), data = list(name = scalar(name))))
    }

    list(status = scalar("ok"))
  }

  resolve_dashboard_template <- function() {
    candidate <- tryCatch(vdb_dashboard_template_path(), error = function(e) "")
    if (nzchar(candidate) && file.exists(candidate)) {
      return(candidate)
    }

    installed <- system.file("dashboard", "index.html", package = "VectrixDB")
    if (nzchar(installed) && file.exists(installed)) {
      return(installed)
    }

    ""
  }

  # Create API
  pr <- plumber::plumber$new()

  # Health check endpoint
  pr$handle("GET", "/health", function(req, res) {
    list(status = scalar("ok"), version = scalar("1.1.2"))
  })

  # Root and auth status
  pr$handle("GET", "/", function(req, res) {
    if (dashboard) {
      res$status <- 302
      res$setHeader("Location", "/dashboard")
      return("")
    }
    list(status = scalar("ok"), version = scalar("1.1.2"))
  })

  pr$handle("GET", "/auth/status", function(req, res) {
    list(
      ok = scalar(TRUE),
      data = list(
        auth_enabled = scalar(auth_enabled),
        read_only = scalar(!auth_enabled)
      )
    )
  })

  # Redirect /docs to Swagger UI
  pr$handle("GET", "/docs", function(req, res) {
    res$status <- 302
    res$setHeader("Location", "/__docs__/")
    return("")
  })

  # List collections
  pr$handle("GET", "/collections", function(req, res) {
    list(collections = list_collection_names())
  })

  # Create collection
  pr$handle("POST", "/collections", function(req, res) {
    create_collection_handler(req, res, api_shape = FALSE)
  })

  # Get collection info
  pr$handle("GET", "/collections/<name>", function(name, req, res) {
    if (!(name %in% list_collection_names())) {
      res$status <- 404
      return(list(error = sprintf("Collection '%s' does not exist", name)))
    }
    collection_info(name)
  })

  # Add documents (legacy)
  pr$handle("POST", "/collections/<name>/add", function(name, req, res) {
    add_handler(name, req, res)
  })

  # Search (legacy)
  pr$handle("POST", "/collections/<name>/search", function(name, req, res) {
    search_handler(name, req, res, mode = "dense")
  })

  # Delete collection
  pr$handle("DELETE", "/collections/<name>", function(name, req, res) {
    delete_collection_handler(name, req, res, api_shape = FALSE)
  })

  # Dashboard-compatible API routes
  pr$handle("GET", "/api/collections", function(req, res) {
    names <- list_collection_names()
    list(collections = lapply(names, collection_info))
  })

  pr$handle("POST", "/api/collections", function(req, res) {
    create_collection_handler(req, res, api_shape = TRUE)
  })

  pr$handle("POST", "/api/v2/collections", function(req, res) {
    create_collection_handler(req, res, api_shape = TRUE)
  })

  pr$handle("GET", "/api/collections/<name>", function(name, req, res) {
    if (!(name %in% list_collection_names())) {
      res$status <- 404
      return(list(detail = sprintf("Collection '%s' does not exist", name)))
    }
    list(ok = scalar(TRUE), data = collection_info(name))
  })

  pr$handle("DELETE", "/api/collections/<name>", function(name, req, res) {
    delete_collection_handler(name, req, res, api_shape = TRUE)
  })

  pr$handle("POST", "/api/v2/collections/<name>/add", function(name, req, res) {
    add_handler(name, req, res)
  })

  pr$handle("POST", "/api/collections/<name>/text-upsert", function(name, req, res) {
    add_handler(name, req, res)
  })

  pr$handle("POST", "/api/collections/<name>/text-search", function(name, req, res) {
    search_handler(name, req, res, mode = "dense")
  })

  pr$handle("POST", "/api/v1/collections/<name>/keyword-search", function(name, req, res) {
    search_handler(name, req, res, mode = "sparse")
  })

  pr$handle("GET", "/api/v1/collections/<name>/points", function(name, req, res) {
    collection <- tryCatch(get_vectrix(name, create = FALSE), error = function(e) NULL)
    if (is.null(collection)) {
      res$status <- 404
      return(list(ok = scalar(FALSE), message = scalar(sprintf("Collection '%s' not found", name))))
    }

    ids <- get_collection_ids(collection)
    limit <- max(1L, as_int(req$argsQuery$limit %||% 20L, 20L))
    offset <- max(0L, as_int(req$argsQuery$offset %||% 0L, 0L))

    if (!length(ids) || offset >= length(ids)) {
      return(list(ok = scalar(TRUE), data = list(total = scalar(length(ids)), ids = character(0))))
    }

    end_idx <- min(length(ids), offset + limit)
    page_ids <- ids[(offset + 1L):end_idx]

    list(
      ok = scalar(TRUE),
      data = list(
        total = scalar(length(ids)),
        limit = scalar(limit),
        offset = scalar(offset),
        ids = page_ids
      )
    )
  })

  pr$handle("GET", "/api/v1/collections/<name>/points/<id>", function(name, id, req, res) {
    collection <- tryCatch(get_vectrix(name, create = FALSE), error = function(e) NULL)
    if (is.null(collection)) {
      res$status <- 404
      return(list(ok = scalar(FALSE), message = scalar(sprintf("Collection '%s' not found", name))))
    }

    point <- get_collection_points(collection, ids = id)
    if (!length(point) || is.null(point[[1]])) {
      res$status <- 404
      return(list(ok = scalar(FALSE), message = scalar(sprintf("Point '%s' not found", id))))
    }

    p <- point[[1]]
    payload <- p$metadata %||% list()
    payload$text <- p$text %||% ""

    list(
      ok = scalar(TRUE),
      data = list(
        id = scalar(id),
        metadata = p$metadata %||% list(),
        payload = payload,
        vector = as.numeric(p$vector %||% numeric(0))
      )
    )
  })

  pr$handle("DELETE", "/api/v1/collections/<name>/points", function(name, req, res) {
    if (!require_auth(req, res, write = TRUE)) {
      return(list(ok = scalar(FALSE), message = scalar("Invalid API key")))
    }

    collection <- tryCatch(get_vectrix(name, create = FALSE), error = function(e) NULL)
    if (is.null(collection)) {
      res$status <- 404
      return(list(ok = scalar(FALSE), message = scalar(sprintf("Collection '%s' not found", name))))
    }

    body <- parse_body(req)
    ids <- as.character(unlist(body$ids %||% list(), use.names = FALSE))
    if (!length(ids)) {
      res$status <- 400
      return(list(ok = scalar(FALSE), message = scalar("No point IDs provided")))
    }

    deleted <- tryCatch({
      collection$delete(ids)
      TRUE
    }, error = function(e) {
      res$status <<- 400
      return(list(error = e$message))
    })

    if (is.list(deleted) && !is.null(deleted$error)) {
      return(list(ok = scalar(FALSE), message = scalar(deleted$error)))
    }

    list(ok = scalar(TRUE), data = list(deleted = scalar(length(ids))), deleted = scalar(length(ids)))
  })

  pr$handle("POST", "/api/v1/collections/<name>/graph/extract", function(name, req, res) {
    if (!require_auth(req, res, write = TRUE)) {
      return(list(detail = "Invalid API key"))
    }

    if (!(name %in% list_collection_names())) {
      res$status <- 404
      return(list(detail = sprintf("Collection '%s' not found", name)))
    }

    body <- parse_body(req)
    force_token <- tolower(as.character(body$force %||% req$argsQuery$force %||% ""))
    force_rebuild <- isTRUE(body$force) || any(force_token %in% c("1", "true", "yes", "y"))

    pipeline <- tryCatch(
      build_graph_pipeline(name, force_rebuild = force_rebuild),
      error = function(e) {
        res$status <<- 500
        return(list(error = e$message))
      }
    )

    if (is.list(pipeline) && !is.null(pipeline$error)) {
      return(list(detail = pipeline$error))
    }

    if (is.null(pipeline)) {
      return(list(
        ok = scalar(TRUE),
        data = list(
          entities_extracted = scalar(0L),
          relationships_extracted = scalar(0L),
          communities = scalar(0L),
          extractor = scalar("regex"),
          message = scalar("No text documents found to extract graph entities")
        )
      ))
    }

    stats <- tryCatch(pipeline$stats(), error = function(e) list())
    extractor <- tryCatch(
      tolower(as.character(pipeline$config$extractor %||% "regex")),
      error = function(e) "regex"
    )

    list(
      ok = scalar(TRUE),
      data = list(
        entities_extracted = scalar(as_int(stats$entity_count %||% 0L, 0L)),
        relationships_extracted = scalar(as_int(stats$relationship_count %||% 0L, 0L)),
        communities = scalar(as_int(stats$community_count %||% 0L, 0L)),
        extractor = scalar(extractor),
        rebuilt = scalar(force_rebuild),
        message = scalar(if (isTRUE(force_rebuild)) "Graph extraction completed (forced rebuild)" else "Graph extraction completed")
      )
    )
  })

  pr$handle("GET", "/api/v1/collections/<name>/graph", function(name, req, res) {
    if (!(name %in% list_collection_names())) {
      res$status <- 404
      return(list(ok = scalar(FALSE), message = scalar(sprintf("Collection '%s' not found", name))))
    }

    limit <- max(1L, min(as_int(req$argsQuery$limit %||% 500L, 500L), 5000L))
    payload <- tryCatch(
      get_graph_payload(name, limit = limit),
      error = function(e) {
        res$status <<- 500
        return(list(error = e$message))
      }
    )

    if (is.list(payload) && !is.null(payload$error)) {
      return(list(ok = scalar(FALSE), message = scalar(payload$error)))
    }

    if (is.null(payload)) {
      return(list(
        ok = scalar(TRUE),
        data = list(
          nodes = list(),
          edges = list(),
          stats = list(
            entity_count = scalar(0L),
            relationship_count = scalar(0L),
            community_count = scalar(0L)
          ),
          message = scalar("No graph data found for this collection")
        )
      ))
    }

    list(
      ok = scalar(TRUE),
      data = list(
        nodes = payload$nodes %||% list(),
        edges = payload$edges %||% list(),
        stats = payload$stats %||% list(),
        message = scalar("Graph loaded")
      )
    )
  })

  # Database stats
  pr$handle("GET", "/stats", function(req, res) {
    names <- list_collection_names()
    list(
      path = data_path,
      storage_type = "memory",
      num_collections = length(names),
      collections = lapply(names, collection_info)
    )
  })

  if (dashboard) {
    dashboard_template <- resolve_dashboard_template()
    if (nzchar(dashboard_template) && file.exists(dashboard_template)) {
      dashboard_html <- paste(
        readLines(dashboard_template, warn = FALSE, encoding = "UTF-8"),
        collapse = "\n"
      )

      pr$handle(
        "GET",
        "/dashboard",
        function(req, res) {
          dashboard_html
        },
        serializer = plumber::serializer_html()
      )
      pr$handle(
        "GET",
        "/dashboard/",
        function(req, res) {
          dashboard_html
        },
        serializer = plumber::serializer_html()
      )
      pr$handle(
        "GET",
        "/dashboard/index.html",
        function(req, res) {
          dashboard_html
        },
        serializer = plumber::serializer_html()
      )
    } else {
      warning("Dashboard template not found. /dashboard will be unavailable.")
    }
  }

  message(sprintf("Starting VectrixDB server at http://%s:%d", host, port))
  message("API documentation available at /docs")
  if (dashboard) {
    message("Dashboard available at /dashboard")
  }

  if (launch.browser) {
    target <- if (dashboard) {
      sprintf("http://%s:%d/dashboard", host, port)
    } else {
      sprintf("http://%s:%d/docs", host, port)
    }
    try(utils::browseURL(target), silent = TRUE)
  }

  # Run server
  pr$run(host = host, port = port)
}

#' Display VectrixDB information
#'
#' @description Show database statistics and info
#'
#' @param path Database path
#'
#' @export
#'
#' @examples
#' \dontrun{
#' vectrix_info("./my_data")
#' }
vectrix_info <- function(path = "./vectrixdb_data") {
  db <- VectrixDB$new(path)
  stats <- db$stats()

  cat("VectrixDB Info\n")
  cat("==============\n")
  cat(sprintf("Path: %s\n", stats$path))
  cat(sprintf("Storage: %s\n", stats$storage_type))
  cat(sprintf("Collections: %d\n", stats$num_collections))

  if (stats$num_collections > 0) {
    cat("\nCollections:\n")
    for (c in stats$collections) {
      cat(sprintf("  - %s: %d docs (%d dims)\n", c$name, c$count, c$dimension))
    }
  }

  invisible(stats)
}
