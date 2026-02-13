#' VectrixDB GraphRAG Module
#'
#' @description Native GraphRAG implementation for VectrixDB
#'
#' Features:
#' - Entity and relationship extraction
#' - Hierarchical community detection
#' - Local, global, and hybrid search strategies
#' - Incremental graph updates
#'
#' @name graphrag
NULL

# =============================================================================
# Enums and Constants
# =============================================================================

#' LLM Provider Types
#' @export
LLMProvider <- list(
  OPENAI = "openai",
  OLLAMA = "ollama",
  AWS_BEDROCK = "aws_bedrock",
  AZURE_OPENAI = "azure_openai"
)

#' Extractor Types
#' @export
ExtractorType <- list(
  LLM = "llm",
  NLP = "nlp",
  HYBRID = "hybrid",
  REGEX = "regex"
)

#' Graph Search Types
#' @export
GraphSearchType <- list(
  LOCAL = "local",
  GLOBAL = "global",
  HYBRID = "hybrid"
)

# =============================================================================
# GraphRAG Configuration
# =============================================================================

#' GraphRAG Configuration
#'
#' @description Configuration for VectrixDB's GraphRAG implementation
#'
#' @examples
#' \dontrun{
#' config <- GraphRAGConfig$new(enabled = TRUE)
#' db <- Vectrix$new("knowledge_base", graphrag_config = config)
#' }
#'
#' @export
GraphRAGConfig <- R6::R6Class(
  "GraphRAGConfig",
  public = list(
    #' @field enabled Whether GraphRAG is enabled
    enabled = FALSE,

    # Chunking
    #' @field chunk_size Target tokens per chunk
    chunk_size = 1200,
    #' @field chunk_overlap Overlapping tokens
    chunk_overlap = 100,
    #' @field chunk_by_sentence Preserve sentence boundaries
    chunk_by_sentence = TRUE,

    # Extraction
    #' @field extractor Extraction method
    extractor = "regex",
    #' @field nlp_model NLP model name
    nlp_model = "en_core_web_sm",

    # LLM
    #' @field llm_provider LLM provider
    llm_provider = "openai",
    #' @field llm_model Model name
    llm_model = "gpt-4o-mini",
    #' @field llm_api_key API key
    llm_api_key = NULL,
    #' @field llm_endpoint Custom endpoint
    llm_endpoint = NULL,
    #' @field llm_temperature Temperature
    llm_temperature = 0.0,
    #' @field llm_max_tokens Max tokens
    llm_max_tokens = 4096,

    # Graph
    #' @field max_community_levels Max hierarchy depth
    max_community_levels = 3,
    #' @field min_community_size Min entities per community
    min_community_size = 5,
    #' @field relationship_threshold Min relationship strength
    relationship_threshold = 0.3,
    #' @field deduplicate_entities Merge similar entities
    deduplicate_entities = TRUE,
    #' @field entity_similarity_threshold Similarity for dedup
    entity_similarity_threshold = 0.85,

    # Retrieval
    #' @field search_type Default search strategy
    search_type = "hybrid",
    #' @field local_search_k Seed entities for local search
    local_search_k = 10,
    #' @field global_search_k Communities for global search
    global_search_k = 5,
    #' @field traversal_depth Max hops
    traversal_depth = 2,
    #' @field include_relationships Include relationship context
    include_relationships = TRUE,
    #' @field include_community_context Include community summaries
    include_community_context = TRUE,

    # Performance
    #' @field enable_incremental Incremental updates
    enable_incremental = TRUE,
    #' @field batch_size Chunks per batch
    batch_size = 50,
    #' @field use_cache Cache embeddings
    use_cache = TRUE,
    #' @field cache_ttl Cache TTL seconds
    cache_ttl = 3600,

    # Entity/Relationship types
    #' @field entity_types Types to extract
    entity_types = NULL,
    #' @field relationship_types Types to extract
    relationship_types = NULL,

    #' @description Create a new GraphRAGConfig
    #' @param enabled Enable GraphRAG
    #' @param ... Additional configuration options
    initialize = function(enabled = FALSE, ...) {
      self$enabled <- enabled
      args <- list(...)
      for (name in names(args)) {
        if (name %in% names(self)) {
          self[[name]] <- args[[name]]
        }
      }

      # Defaults
      if (is.null(self$entity_types)) {
        self$entity_types <- c("PERSON", "ORGANIZATION", "LOCATION", "CONCEPT",
                               "EVENT", "OBJECT", "TECHNOLOGY", "PRODUCT")
      }
      if (is.null(self$relationship_types)) {
        self$relationship_types <- c("RELATED_TO", "WORKS_FOR", "LOCATED_IN",
                                     "PART_OF", "CREATED_BY", "USED_BY", "MENTIONS")
      }

      private$validate()
    },

    #' @description Configure for OpenAI
    #' @param model Model name
    #' @param api_key API key
    #' @return Self
    with_openai = function(model = "gpt-4o-mini", api_key = NULL) {
      self$llm_provider <- LLMProvider$OPENAI
      self$llm_model <- model
      self$llm_api_key <- api_key
      invisible(self)
    },

    #' @description Configure for Ollama
    #' @param model Model name
    #' @param endpoint Endpoint URL
    #' @return Self
    with_ollama = function(model = "llama3.2", endpoint = "http://localhost:11434") {
      self$llm_provider <- LLMProvider$OLLAMA
      self$llm_model <- model
      self$llm_endpoint <- endpoint
      invisible(self)
    }
  ),

  private = list(
    validate = function() {
      if (self$chunk_size < 100) {
        stop("chunk_size must be at least 100 tokens")
      }
      if (self$chunk_overlap >= self$chunk_size) {
        stop("chunk_overlap must be less than chunk_size")
      }
      if (self$relationship_threshold < 0 || self$relationship_threshold > 1) {
        stop("relationship_threshold must be between 0 and 1")
      }
    }
  )
)

# =============================================================================
# Text Unit (Chunk)
# =============================================================================

#' Text Unit
#'
#' @description A chunk of text from a document
#' @export
TextUnit <- R6::R6Class(
  "TextUnit",
  public = list(
    #' @field id Unique identifier
    id = NULL,
    #' @field text Text content
    text = NULL,
    #' @field document_id Source document
    document_id = NULL,
    #' @field chunk_index Index in document
    chunk_index = 0,
    #' @field start_char Start position
    start_char = 0,
    #' @field end_char End position
    end_char = 0,
    #' @field metadata Additional metadata
    metadata = NULL,

    #' @description Create a new TextUnit
    #' @param id Unique ID
    #' @param text Content
    #' @param document_id Source doc
    #' @param chunk_index Index
    #' @param start_char Start
    #' @param end_char End
    #' @param metadata Metadata
    initialize = function(id, text, document_id = NULL, chunk_index = 0,
                          start_char = 0, end_char = 0, metadata = NULL) {
      self$id <- id
      self$text <- text
      self$document_id <- document_id
      self$chunk_index <- chunk_index
      self$start_char <- start_char
      self$end_char <- end_char
      self$metadata <- metadata %||% list()
    }
  )
)

#' Document Chunker
#'
#' @description Splits documents into text units
#' @export
DocumentChunker <- R6::R6Class(
  "DocumentChunker",
  public = list(
    #' @field chunk_size Target chunk size
    chunk_size = 1200,
    #' @field chunk_overlap Overlap size
    chunk_overlap = 100,
    #' @field by_sentence Preserve sentences
    by_sentence = TRUE,

    #' @description Create a new DocumentChunker
    #' @param chunk_size Target size
    #' @param chunk_overlap Overlap
    #' @param by_sentence Preserve sentences
    initialize = function(chunk_size = 1200, chunk_overlap = 100, by_sentence = TRUE) {
      self$chunk_size <- chunk_size
      self$chunk_overlap <- chunk_overlap
      self$by_sentence <- by_sentence
    },

    #' @description Chunk a document
    #' @param text Document text
    #' @param document_id Document ID
    #' @return List of TextUnit objects
    chunk = function(text, document_id = NULL) {
      if (is.null(text) || nchar(text) == 0) {
        return(list())
      }

      if (self$by_sentence) {
        return(private$chunk_by_sentences(text, document_id))
      } else {
        return(private$chunk_by_chars(text, document_id))
      }
    }
  ),

  private = list(
    chunk_by_sentences = function(text, document_id) {
      # Split by sentence endings
      sentences <- unlist(strsplit(text, "(?<=[.!?])\\s+", perl = TRUE))
      sentences <- sentences[nchar(trimws(sentences)) > 0]

      if (length(sentences) == 0) {
        return(list())
      }

      chunks <- list()
      current_chunk <- ""
      current_start <- 1
      chunk_index <- 0

      for (sentence in sentences) {
        if (nchar(current_chunk) + nchar(sentence) > self$chunk_size && nchar(current_chunk) > 0) {
          # Save current chunk
          chunk_index <- chunk_index + 1
          chunks[[chunk_index]] <- TextUnit$new(
            id = paste0(document_id %||% "doc", "_chunk_", chunk_index),
            text = trimws(current_chunk),
            document_id = document_id,
            chunk_index = chunk_index,
            start_char = current_start,
            end_char = current_start + nchar(current_chunk) - 1
          )

          # Start new chunk with overlap
          overlap_text <- private$get_overlap(current_chunk)
          current_chunk <- paste0(overlap_text, " ", sentence)
          current_start <- current_start + nchar(current_chunk) - nchar(overlap_text)
        } else {
          current_chunk <- paste(current_chunk, sentence, sep = " ")
        }
      }

      # Add final chunk
      if (nchar(trimws(current_chunk)) > 0) {
        chunk_index <- chunk_index + 1
        chunks[[chunk_index]] <- TextUnit$new(
          id = paste0(document_id %||% "doc", "_chunk_", chunk_index),
          text = trimws(current_chunk),
          document_id = document_id,
          chunk_index = chunk_index,
          start_char = current_start,
          end_char = nchar(text)
        )
      }

      chunks
    },

    chunk_by_chars = function(text, document_id) {
      chunks <- list()
      start <- 1
      chunk_index <- 0
      text_len <- nchar(text)

      while (start <= text_len) {
        end <- min(start + self$chunk_size - 1, text_len)
        chunk_text <- substr(text, start, end)

        chunk_index <- chunk_index + 1
        chunks[[chunk_index]] <- TextUnit$new(
          id = paste0(document_id %||% "doc", "_chunk_", chunk_index),
          text = chunk_text,
          document_id = document_id,
          chunk_index = chunk_index,
          start_char = start,
          end_char = end
        )

        start <- end + 1 - self$chunk_overlap
      }

      chunks
    },

    get_overlap = function(text) {
      if (nchar(text) <= self$chunk_overlap) {
        return(text)
      }
      substr(text, nchar(text) - self$chunk_overlap + 1, nchar(text))
    }
  )
)

# =============================================================================
# Entity and Relationship
# =============================================================================

#' Entity
#'
#' @description An extracted entity
#' @export
Entity <- R6::R6Class(
  "Entity",
  public = list(
    #' @field id Unique identifier
    id = NULL,
    #' @field name Entity name
    name = NULL,
    #' @field type Entity type
    type = NULL,
    #' @field description Description
    description = NULL,
    #' @field source_chunks Source chunk IDs
    source_chunks = NULL,
    #' @field embedding Vector embedding
    embedding = NULL,
    #' @field metadata Additional metadata
    metadata = NULL,

    #' @description Create a new Entity
    #' @param id Unique ID
    #' @param name Name
    #' @param type Type
    #' @param description Description
    #' @param source_chunks Sources
    #' @param embedding Vector
    #' @param metadata Metadata
    initialize = function(id = NULL, name, type, description = NULL,
                          source_chunks = NULL, embedding = NULL, metadata = NULL) {
      self$id <- id %||% digest::digest(paste0(name, type), algo = "md5")
      self$name <- name
      self$type <- type
      self$description <- description
      self$source_chunks <- source_chunks %||% character(0)
      self$embedding <- embedding
      self$metadata <- metadata %||% list()
    },

    #' @description Convert to list
    to_list = function() {
      list(
        id = self$id,
        name = self$name,
        type = self$type,
        description = self$description,
        source_chunks = self$source_chunks,
        metadata = self$metadata
      )
    }
  )
)

#' Relationship
#'
#' @description A relationship between entities
#' @export
Relationship <- R6::R6Class(
  "Relationship",
  public = list(
    #' @field id Unique identifier
    id = NULL,
    #' @field source_id Source entity ID
    source_id = NULL,
    #' @field target_id Target entity ID
    target_id = NULL,
    #' @field type Relationship type
    type = NULL,
    #' @field description Description
    description = NULL,
    #' @field weight Relationship weight
    weight = 1.0,
    #' @field source_chunks Source chunk IDs
    source_chunks = NULL,
    #' @field metadata Additional metadata
    metadata = NULL,

    #' @description Create a new Relationship
    #' @param source_id Source entity
    #' @param target_id Target entity
    #' @param type Relationship type
    #' @param description Description
    #' @param weight Weight
    #' @param source_chunks Sources
    #' @param metadata Metadata
    initialize = function(source_id, target_id, type, description = NULL,
                          weight = 1.0, source_chunks = NULL, metadata = NULL) {
      self$id <- digest::digest(paste0(source_id, target_id, type), algo = "md5")
      self$source_id <- source_id
      self$target_id <- target_id
      self$type <- type
      self$description <- description
      self$weight <- weight
      self$source_chunks <- source_chunks %||% character(0)
      self$metadata <- metadata %||% list()
    },

    #' @description Convert to list
    to_list = function() {
      list(
        id = self$id,
        source_id = self$source_id,
        target_id = self$target_id,
        type = self$type,
        description = self$description,
        weight = self$weight,
        source_chunks = self$source_chunks,
        metadata = self$metadata
      )
    }
  )
)

#' Extraction Result
#'
#' @description Result of entity extraction
#' @export
ExtractionResult <- R6::R6Class(
  "ExtractionResult",
  public = list(
    #' @field entities List of Entity objects
    entities = NULL,
    #' @field relationships List of Relationship objects
    relationships = NULL,
    #' @field source_chunk Source chunk ID
    source_chunk = NULL,

    #' @description Create new ExtractionResult
    #' @param entities Entities
    #' @param relationships Relationships
    #' @param source_chunk Source
    initialize = function(entities = list(), relationships = list(), source_chunk = NULL) {
      self$entities <- entities
      self$relationships <- relationships
      self$source_chunk <- source_chunk
    }
  )
)

# =============================================================================
# Entity Extractor (Regex-based for R)
# =============================================================================

#' Regex Entity Extractor
#'
#' @description Simple regex-based entity extractor (no external dependencies)
#' @export
RegexExtractor <- R6::R6Class(
  "RegexExtractor",
  public = list(
    #' @field entity_types Entity types to extract
    entity_types = NULL,

    #' @description Create a new RegexExtractor
    #' @param entity_types Types to extract
    initialize = function(entity_types = NULL) {
      self$entity_types <- entity_types %||% c("PERSON", "ORGANIZATION", "LOCATION")
    },

    #' @description Extract entities from text
    #' @param text Text to extract from
    #' @param chunk_id Chunk ID
    #' @return ExtractionResult
    extract = function(text, chunk_id = NULL) {
      entities <- list()
      relationships <- list()

      # Extract capitalized phrases as potential entities
      # Pattern: 2+ capitalized words or single capitalized word followed by uppercase
      pattern <- "\\b[A-Z][a-z]+(?:\\s+[A-Z][a-z]+)+\\b|\\b[A-Z]{2,}\\b"
      matches <- regmatches(text, gregexpr(pattern, text, perl = TRUE))[[1]]
      matches <- unique(matches)

      for (match in matches) {
        entity_type <- private$classify_entity(match)
        entity <- Entity$new(
          name = match,
          type = entity_type,
          source_chunks = c(chunk_id)
        )
        entities[[length(entities) + 1]] <- entity
      }

      # Build entity lookup by name
      entity_lookup <- list()
      entity_names <- character(0)
      for (entity in entities) {
        entity_lookup[[entity$name]] <- entity
        entity_names <- c(entity_names, entity$name)
      }

      # Extract relationships by finding entity co-occurrences
      # Sort by length (longest first) to match multi-word entities first
      entity_names <- entity_names[order(nchar(entity_names), decreasing = TRUE)]

      # Relationship patterns (connector words)
      connectors <- list(
        c("\\s+and\\s+", "RELATED_TO"),
        c("\\s+with\\s+", "RELATED_TO"),
        c("\\s+works?\\s+(?:for|at|with)\\s+", "WORKS_FOR"),
        c("\\s+(?:is|was|are|were)\\s+(?:CEO|founder|president|director)\\s+(?:of|at)\\s+", "WORKS_FOR"),
        c("\\s+founded\\s+", "FOUNDED"),
        c("\\s+created\\s+", "CREATED"),
        c("\\s+(?:is|are|was|were)\\s+(?:in|at|from|located)\\s+", "LOCATED_IN"),
        c("\\s+releases?d?\\s+", "CREATED"),
        c("\\s+produces?d?\\s+", "CREATED")
      )

      # Passive voice patterns (Y was Xed by Z) - note: [.,]? allows optional punctuation
      passive_patterns <- list(
        c("[.,]?\\s+(?:was|were)\\s+founded\\s+by\\s+", "FOUNDED"),
        c("[.,]?\\s+(?:was|were)\\s+created\\s+by\\s+", "CREATED"),
        c("[.,]?\\s+(?:was|were)\\s+(?:led|run|managed)\\s+by\\s+", "WORKS_FOR")
      )

      # Check passive patterns (reverse direction)
      for (i in seq_along(entity_names)) {
        for (j in seq_along(entity_names)) {
          if (i >= j) next

          source_name <- entity_names[i]
          target_name <- entity_names[j]

          for (pp in passive_patterns) {
            pattern <- pp[1]
            rel_type <- pp[2]

            # "Target was founded by Source"
            passive_pattern <- paste0(gsub("([.|()\\^{}+$*?\\[\\]])", "\\\\\\1", target_name),
                                      pattern,
                                      gsub("([.|()\\^{}+$*?\\[\\]])", "\\\\\\1", source_name))

            if (grepl(passive_pattern, text, perl = TRUE, ignore.case = TRUE)) {
              rel <- Relationship$new(
                source_id = entity_lookup[[source_name]]$id,
                target_id = entity_lookup[[target_name]]$id,
                type = rel_type,
                source_chunks = c(chunk_id)
              )
              relationships[[length(relationships) + 1]] <- rel
            }
          }
        }
      }

      for (i in seq_along(entity_names)) {
        for (j in seq_along(entity_names)) {
          if (i >= j) next  # Avoid duplicates and self-relations

          source_name <- entity_names[i]
          target_name <- entity_names[j]

          for (conn in connectors) {
            pattern <- conn[1]
            rel_type <- conn[2]

            # Check both directions
            pattern1 <- paste0(gsub("([.|()\\^{}+$*?\\[\\]])", "\\\\\\1", source_name),
                               pattern,
                               gsub("([.|()\\^{}+$*?\\[\\]])", "\\\\\\1", target_name))
            pattern2 <- paste0(gsub("([.|()\\^{}+$*?\\[\\]])", "\\\\\\1", target_name),
                               pattern,
                               gsub("([.|()\\^{}+$*?\\[\\]])", "\\\\\\1", source_name))

            if (grepl(pattern1, text, perl = TRUE, ignore.case = TRUE)) {
              rel <- Relationship$new(
                source_id = entity_lookup[[source_name]]$id,
                target_id = entity_lookup[[target_name]]$id,
                type = rel_type,
                source_chunks = c(chunk_id)
              )
              relationships[[length(relationships) + 1]] <- rel
            } else if (grepl(pattern2, text, perl = TRUE, ignore.case = TRUE)) {
              rel <- Relationship$new(
                source_id = entity_lookup[[target_name]]$id,
                target_id = entity_lookup[[source_name]]$id,
                type = rel_type,
                source_chunks = c(chunk_id)
              )
              relationships[[length(relationships) + 1]] <- rel
            }
          }
        }
      }

      ExtractionResult$new(
        entities = entities,
        relationships = relationships,
        source_chunk = chunk_id
      )
    }
  ),

  private = list(
    classify_entity = function(text) {
      # Simple classification based on patterns
      person_indicators <- c("Mr", "Mrs", "Ms", "Dr", "Prof")
      org_indicators <- c("Inc", "Corp", "LLC", "Ltd", "Company", "Group", "Association")
      loc_indicators <- c("City", "State", "Country", "Street", "Avenue", "Road")

      for (ind in person_indicators) {
        if (grepl(ind, text, ignore.case = TRUE)) return("PERSON")
      }
      for (ind in org_indicators) {
        if (grepl(ind, text, ignore.case = TRUE)) return("ORGANIZATION")
      }
      for (ind in loc_indicators) {
        if (grepl(ind, text, ignore.case = TRUE)) return("LOCATION")
      }

      # Default based on word count
      if (length(strsplit(text, "\\s+")[[1]]) >= 2) {
        return("PERSON")  # Multi-word = likely person name
      }
      "CONCEPT"
    }
  )
)

# =============================================================================
# Knowledge Graph
# =============================================================================

#' Knowledge Graph
#'
#' @description Graph storage for entities and relationships
#' @export
KnowledgeGraph <- R6::R6Class(
  "KnowledgeGraph",
  public = list(
    #' @field name Graph name
    name = NULL,

    #' @description Create a new KnowledgeGraph
    #' @param name Graph name
    initialize = function(name = "default") {
      self$name <- name
      private$entities <- list()
      private$relationships <- list()
      private$adjacency <- list()
    },

    #' @description Add an entity
    #' @param entity Entity object
    add_entity = function(entity) {
      private$entities[[entity$id]] <- entity
      if (is.null(private$adjacency[[entity$id]])) {
        private$adjacency[[entity$id]] <- list(in_edges = list(), out_edges = list())
      }
      invisible(self)
    },

    #' @description Add a relationship
    #' @param relationship Relationship object
    add_relationship = function(relationship) {
      private$relationships[[relationship$id]] <- relationship

      # Update adjacency
      if (is.null(private$adjacency[[relationship$source_id]])) {
        private$adjacency[[relationship$source_id]] <- list(in_edges = list(), out_edges = list())
      }
      if (is.null(private$adjacency[[relationship$target_id]])) {
        private$adjacency[[relationship$target_id]] <- list(in_edges = list(), out_edges = list())
      }

      private$adjacency[[relationship$source_id]]$out_edges[[relationship$id]] <- relationship
      private$adjacency[[relationship$target_id]]$in_edges[[relationship$id]] <- relationship

      invisible(self)
    },

    #' @description Get entity by ID
    #' @param entity_id Entity ID
    #' @return Entity or NULL
    get_entity = function(entity_id) {
      private$entities[[entity_id]]
    },

    #' @description Get all entities
    #' @return List of Entity objects
    get_all_entities = function() {
      private$entities
    },

    #' @description Get all relationships
    #' @return List of Relationship objects
    get_all_relationships = function() {
      private$relationships
    },

    #' @description Get neighbors of an entity
    #' @param entity_id Entity ID
    #' @param direction "out", "in", or "both"
    #' @return List of Entity objects
    get_neighbors = function(entity_id, direction = "both") {
      adj <- private$adjacency[[entity_id]]
      if (is.null(adj)) return(list())

      neighbor_ids <- c()
      if (direction %in% c("out", "both")) {
        for (rel in adj$out_edges) {
          neighbor_ids <- c(neighbor_ids, rel$target_id)
        }
      }
      if (direction %in% c("in", "both")) {
        for (rel in adj$in_edges) {
          neighbor_ids <- c(neighbor_ids, rel$source_id)
        }
      }

      neighbor_ids <- unique(neighbor_ids)
      lapply(neighbor_ids, function(id) private$entities[[id]])
    },

    #' @description Traverse graph from seed entities
    #' @param seed_ids Starting entity IDs
    #' @param max_depth Maximum depth
    #' @return SubGraph object
    traverse = function(seed_ids, max_depth = 2) {
      visited <- character(0)
      result_entities <- list()
      result_relationships <- list()

      queue <- as.list(seed_ids)
      depth <- 0

      while (length(queue) > 0 && depth < max_depth) {
        next_queue <- list()

        for (entity_id in queue) {
          if (entity_id %in% visited) next
          visited <- c(visited, entity_id)

          entity <- private$entities[[entity_id]]
          if (!is.null(entity)) {
            result_entities[[entity_id]] <- entity
          }

          # Get edges
          adj <- private$adjacency[[entity_id]]
          if (!is.null(adj)) {
            for (rel in c(adj$out_edges, adj$in_edges)) {
              result_relationships[[rel$id]] <- rel
              other_id <- if (rel$source_id == entity_id) rel$target_id else rel$source_id
              if (!(other_id %in% visited)) {
                next_queue <- c(next_queue, other_id)
              }
            }
          }
        }

        queue <- unique(next_queue)
        depth <- depth + 1
      }

      SubGraph$new(
        entities = result_entities,
        relationships = result_relationships
      )
    },

    #' @description Get entity count
    #' @return Integer
    entity_count = function() {
      length(private$entities)
    },

    #' @description Get relationship count
    #' @return Integer
    relationship_count = function() {
      length(private$relationships)
    },

    #' @description Search entities by name
    #' @param query Query string
    #' @param limit Max results
    #' @return List of Entity objects
    search_entities = function(query, limit = 10) {
      query_lower <- tolower(query)
      matches <- list()

      for (entity in private$entities) {
        if (grepl(query_lower, tolower(entity$name), fixed = TRUE)) {
          matches[[length(matches) + 1]] <- entity
        }
        if (length(matches) >= limit) break
      }

      matches
    }
  ),

  private = list(
    entities = NULL,
    relationships = NULL,
    adjacency = NULL
  )
)

#' SubGraph
#'
#' @description A subset of a knowledge graph
#' @export
SubGraph <- R6::R6Class(
  "SubGraph",
  public = list(
    #' @field entities Entities in subgraph
    entities = NULL,
    #' @field relationships Relationships in subgraph
    relationships = NULL,

    #' @description Create a new SubGraph
    #' @param entities Entities
    #' @param relationships Relationships
    initialize = function(entities = list(), relationships = list()) {
      self$entities <- entities
      self$relationships <- relationships
    },

    #' @description Convert to list
    to_list = function() {
      list(
        entities = lapply(self$entities, function(e) e$to_list()),
        relationships = lapply(self$relationships, function(r) r$to_list())
      )
    }
  )
)

# =============================================================================
# Community Detection
# =============================================================================

#' Community
#'
#' @description A community of entities
#' @export
Community <- R6::R6Class(
  "Community",
  public = list(
    #' @field id Community ID
    id = NULL,
    #' @field level Hierarchy level
    level = 0,
    #' @field entity_ids Member entity IDs
    entity_ids = NULL,
    #' @field summary Community summary
    summary = NULL,
    #' @field parent_id Parent community ID
    parent_id = NULL,
    #' @field child_ids Child community IDs
    child_ids = NULL,

    #' @description Create a new Community
    #' @param id ID
    #' @param level Level
    #' @param entity_ids Members
    #' @param summary Summary
    #' @param parent_id Parent
    initialize = function(id, level = 0, entity_ids = character(0),
                          summary = NULL, parent_id = NULL) {
      self$id <- id
      self$level <- level
      self$entity_ids <- entity_ids
      self$summary <- summary
      self$parent_id <- parent_id
      self$child_ids <- character(0)
    },

    #' @description Get size
    #' @return Integer
    size = function() {
      length(self$entity_ids)
    }
  )
)

#' Simple Community Detector
#'
#' @description Detects communities using connected components
#' @export
CommunityDetector <- R6::R6Class(
  "CommunityDetector",
  public = list(
    #' @field min_size Minimum community size
    min_size = 5,
    #' @field max_levels Maximum hierarchy levels
    max_levels = 3,

    #' @description Create a new CommunityDetector
    #' @param min_size Min size
    #' @param max_levels Max levels
    initialize = function(min_size = 5, max_levels = 3) {
      self$min_size <- min_size
      self$max_levels <- max_levels
    },

    #' @description Detect communities in graph
    #' @param graph KnowledgeGraph object
    #' @return List of Community objects
    detect = function(graph) {
      entities <- graph$get_all_entities()
      if (length(entities) == 0) return(list())

      # Simple connected components algorithm
      visited <- character(0)
      communities <- list()
      community_id <- 0

      for (entity_id in names(entities)) {
        if (entity_id %in% visited) next

        # BFS to find connected component
        component <- c()
        queue <- c(entity_id)

        while (length(queue) > 0) {
          current <- queue[1]
          queue <- queue[-1]

          if (current %in% visited) next
          visited <- c(visited, current)
          component <- c(component, current)

          neighbors <- graph$get_neighbors(current)
          for (neighbor in neighbors) {
            if (!is.null(neighbor) && !(neighbor$id %in% visited)) {
              queue <- c(queue, neighbor$id)
            }
          }
        }

        if (length(component) >= self$min_size) {
          community_id <- community_id + 1
          communities[[community_id]] <- Community$new(
            id = paste0("community_", community_id),
            level = 0,
            entity_ids = component
          )
        }
      }

      communities
    }
  )
)

# =============================================================================
# Graph Search
# =============================================================================

#' Local Search Result
#'
#' @description Result from local graph search
#' @export
LocalSearchResult <- R6::R6Class(
  "LocalSearchResult",
  public = list(
    #' @field entities Matching entities
    entities = NULL,
    #' @field relationships Related relationships
    relationships = NULL,
    #' @field subgraph Traversed subgraph
    subgraph = NULL,
    #' @field context Combined context text
    context = NULL,
    #' @field score Relevance score
    score = 0,

    #' @description Create new LocalSearchResult
    #' @param entities Entities
    #' @param relationships Relationships
    #' @param subgraph SubGraph
    #' @param context Context
    #' @param score Score
    initialize = function(entities = list(), relationships = list(),
                          subgraph = NULL, context = NULL, score = 0) {
      self$entities <- entities
      self$relationships <- relationships
      self$subgraph <- subgraph
      self$context <- context
      self$score <- score
    }
  )
)

#' Global Search Result
#'
#' @description Result from global community search
#' @export
GlobalSearchResult <- R6::R6Class(
  "GlobalSearchResult",
  public = list(
    #' @field communities Matching communities
    communities = NULL,
    #' @field summaries Community summaries
    summaries = NULL,
    #' @field context Combined context
    context = NULL,
    #' @field score Relevance score
    score = 0,

    #' @description Create new GlobalSearchResult
    #' @param communities Communities
    #' @param summaries Summaries
    #' @param context Context
    #' @param score Score
    initialize = function(communities = list(), summaries = character(0),
                          context = NULL, score = 0) {
      self$communities <- communities
      self$summaries <- summaries
      self$context <- context
      self$score <- score
    }
  )
)

#' Local Searcher
#'
#' @description Entity-based graph search
#' @export
LocalSearcher <- R6::R6Class(
  "LocalSearcher",
  public = list(
    #' @field graph Knowledge graph
    graph = NULL,
    #' @field k Number of seed entities
    k = 10,
    #' @field traversal_depth Max hops
    traversal_depth = 2,

    #' @description Create a new LocalSearcher
    #' @param graph KnowledgeGraph
    #' @param k Seed entities
    #' @param traversal_depth Max depth
    initialize = function(graph, k = 10, traversal_depth = 2) {
      self$graph <- graph
      self$k <- k
      self$traversal_depth <- traversal_depth
    },

    #' @description Search the graph
    #' @param query Query string
    #' @return LocalSearchResult
    search = function(query) {
      # Find seed entities matching query
      seeds <- self$graph$search_entities(query, self$k)
      if (length(seeds) == 0) {
        return(LocalSearchResult$new())
      }

      # Traverse from seeds
      seed_ids <- sapply(seeds, function(e) e$id)
      subgraph <- self$graph$traverse(seed_ids, self$traversal_depth)

      # Build context
      context_parts <- c()
      for (entity in subgraph$entities) {
        context_parts <- c(context_parts, sprintf("%s (%s)", entity$name, entity$type))
      }
      for (rel in subgraph$relationships) {
        source <- self$graph$get_entity(rel$source_id)
        target <- self$graph$get_entity(rel$target_id)
        if (!is.null(source) && !is.null(target)) {
          context_parts <- c(context_parts,
                            sprintf("%s -[%s]-> %s", source$name, rel$type, target$name))
        }
      }

      LocalSearchResult$new(
        entities = subgraph$entities,
        relationships = subgraph$relationships,
        subgraph = subgraph,
        context = paste(context_parts, collapse = "\n"),
        score = length(seeds) / self$k
      )
    }
  )
)

#' Global Searcher
#'
#' @description Community-based graph search
#' @export
GlobalSearcher <- R6::R6Class(
  "GlobalSearcher",
  public = list(
    #' @field communities List of communities
    communities = NULL,
    #' @field k Number of communities
    k = 5,

    #' @description Create a new GlobalSearcher
    #' @param communities List of Community objects
    #' @param k Number of communities
    initialize = function(communities, k = 5) {
      self$communities <- communities
      self$k <- k
    },

    #' @description Search communities
    #' @param query Query string
    #' @return GlobalSearchResult
    search = function(query) {
      if (length(self$communities) == 0) {
        return(GlobalSearchResult$new())
      }

      # Simple: return top communities by size
      sorted_communities <- self$communities[order(-sapply(self$communities, function(c) c$size()))]
      top_communities <- head(sorted_communities, self$k)

      summaries <- sapply(top_communities, function(c) c$summary %||% "")

      GlobalSearchResult$new(
        communities = top_communities,
        summaries = summaries,
        context = paste(summaries[summaries != ""], collapse = "\n\n"),
        score = 1.0
      )
    }
  )
)

# =============================================================================
# GraphRAG Pipeline
# =============================================================================

#' GraphRAG Pipeline
#'
#' @description Complete GraphRAG processing pipeline
#' @export
GraphRAGPipeline <- R6::R6Class(
  "GraphRAGPipeline",
  public = list(
    #' @field config GraphRAGConfig
    config = NULL,
    #' @field graph KnowledgeGraph
    graph = NULL,
    #' @field communities Detected communities
    communities = NULL,

    #' @description Create a new GraphRAGPipeline
    #' @param config GraphRAGConfig
    initialize = function(config = NULL) {
      self$config <- config %||% GraphRAGConfig$new(enabled = TRUE)
      self$graph <- KnowledgeGraph$new()
      self$communities <- list()

      private$chunker <- DocumentChunker$new(
        chunk_size = self$config$chunk_size,
        chunk_overlap = self$config$chunk_overlap,
        by_sentence = self$config$chunk_by_sentence
      )

      private$extractor <- RegexExtractor$new(
        entity_types = self$config$entity_types
      )

      private$detector <- CommunityDetector$new(
        min_size = self$config$min_community_size,
        max_levels = self$config$max_community_levels
      )
    },

    #' @description Process documents
    #' @param texts Character vector of documents
    #' @param document_ids Document IDs
    #' @return Self
    process = function(texts, document_ids = NULL) {
      if (length(texts) == 0) return(invisible(self))

      if (is.null(document_ids)) {
        document_ids <- paste0("doc_", seq_along(texts))
      }

      # Chunk documents
      all_chunks <- list()
      for (i in seq_along(texts)) {
        chunks <- private$chunker$chunk(texts[i], document_ids[i])
        all_chunks <- c(all_chunks, chunks)
      }

      # Extract entities and relationships
      for (chunk in all_chunks) {
        result <- private$extractor$extract(chunk$text, chunk$id)

        for (entity in result$entities) {
          self$graph$add_entity(entity)
        }
        for (rel in result$relationships) {
          self$graph$add_relationship(rel)
        }
      }

      # Detect communities
      self$communities <- private$detector$detect(self$graph)

      invisible(self)
    },

    #' @description Search the graph
    #' @param query Query string
    #' @param search_type "local", "global", or "hybrid"
    #' @return Search result
    search = function(query, search_type = NULL) {
      search_type <- search_type %||% self$config$search_type

      if (search_type == "local") {
        searcher <- LocalSearcher$new(
          self$graph,
          k = self$config$local_search_k,
          traversal_depth = self$config$traversal_depth
        )
        return(searcher$search(query))
      } else if (search_type == "global") {
        searcher <- GlobalSearcher$new(
          self$communities,
          k = self$config$global_search_k
        )
        return(searcher$search(query))
      } else {
        # Hybrid: combine local and global
        local_searcher <- LocalSearcher$new(
          self$graph,
          k = self$config$local_search_k,
          traversal_depth = self$config$traversal_depth
        )
        global_searcher <- GlobalSearcher$new(
          self$communities,
          k = self$config$global_search_k
        )

        local_result <- local_searcher$search(query)
        global_result <- global_searcher$search(query)

        # Combine contexts
        combined_context <- paste(
          c(local_result$context, global_result$context),
          collapse = "\n\n---\n\n"
        )

        list(
          local = local_result,
          global = global_result,
          context = combined_context,
          score = (local_result$score + global_result$score) / 2
        )
      }
    },

    #' @description Get statistics
    #' @return Named list
    stats = function() {
      list(
        entity_count = self$graph$entity_count(),
        relationship_count = self$graph$relationship_count(),
        community_count = length(self$communities)
      )
    }
  ),

  private = list(
    chunker = NULL,
    extractor = NULL,
    detector = NULL
  )
)

#' Create GraphRAG Pipeline
#'
#' @description Factory function for GraphRAGPipeline
#' @param config GraphRAGConfig (optional)
#' @return GraphRAGPipeline object
#' @export
create_pipeline <- function(config = NULL) {
  GraphRAGPipeline$new(config)
}

#' Create Default GraphRAG Config
#'
#' @description Create default config with regex extractor
#' @param ... Additional options
#' @return GraphRAGConfig object
#' @export
create_default_graphrag_config <- function(...) {
  GraphRAGConfig$new(enabled = TRUE, extractor = "regex", ...)
}

#' Null coalescing for graphrag.R
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a
