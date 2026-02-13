#' VectrixDB Advanced Search Features
#'
#' @description Enterprise-grade search capabilities:
#' - Faceted search with aggregations
#' - ACL/Security filtering
#' - Text analyzers (stemming, synonyms, stopwords)
#'
#' @name advanced_search
NULL

# =============================================================================
# Faceted Search
# =============================================================================

#' Facet Configuration
#'
#' @description Configuration for a facet field
#' @export
FacetConfig <- R6::R6Class(
  "FacetConfig",
  public = list(
    #' @field field Field name to facet on
    field = NULL,
    #' @field limit Max values to return
    limit = 10,
    #' @field min_count Minimum count to include
    min_count = 1,
    #' @field sort_by Sort by "count" or "value"
    sort_by = "count",
    #' @field include_zero Include zero-count values
    include_zero = FALSE,

    #' @description Create a new FacetConfig
    #' @param field Field name
    #' @param limit Max values (default: 10)
    #' @param min_count Min count (default: 1)
    #' @param sort_by Sort method (default: "count")
    #' @param include_zero Include zeros (default: FALSE)
    initialize = function(field,
                          limit = 10,
                          min_count = 1,
                          sort_by = "count",
                          include_zero = FALSE) {
      self$field <- field
      self$limit <- limit
      self$min_count <- min_count
      self$sort_by <- sort_by
      self$include_zero <- include_zero
    }
  )
)

#' Facet Value
#'
#' @description A single facet value with count
#' @export
FacetValue <- R6::R6Class(
  "FacetValue",
  public = list(
    #' @field value The facet value
    value = NULL,
    #' @field count Number of occurrences
    count = 0,

    #' @description Create a new FacetValue
    #' @param value The value
    #' @param count The count
    initialize = function(value, count) {
      self$value <- value
      self$count <- count
    }
  )
)

#' Facet Result
#'
#' @description Result of facet aggregation
#' @export
FacetResult <- R6::R6Class(
  "FacetResult",
  public = list(
    #' @field field Field name
    field = NULL,
    #' @field values List of FacetValue objects
    values = NULL,
    #' @field total_count Total count
    total_count = 0,
    #' @field other_count Count of values not in top-N
    other_count = 0,

    #' @description Create a new FacetResult
    #' @param field Field name
    #' @param values List of FacetValue objects
    #' @param total_count Total count
    #' @param other_count Other count
    initialize = function(field, values, total_count, other_count = 0) {
      self$field <- field
      self$values <- values
      self$total_count <- total_count
      self$other_count <- other_count
    },

    #' @description Convert to list
    to_list = function() {
      list(
        field = self$field,
        values = lapply(self$values, function(v) list(value = v$value, count = v$count)),
        total_count = self$total_count,
        other_count = self$other_count
      )
    }
  )
)

#' Facet Aggregator
#'
#' @description Faceted search aggregator for computing aggregations/counts
#'
#' @examples
#' \dontrun{
#' aggregator <- FacetAggregator$new()
#' facets <- aggregator$aggregate(
#'   documents = list(
#'     list(category = "tech", author = "Alice"),
#'     list(category = "science", author = "Bob")
#'   ),
#'   facet_fields = c("category", "author")
#' )
#' }
#'
#' @export
FacetAggregator <- R6::R6Class(
  "FacetAggregator",
  public = list(
    #' @description Aggregate facet values from documents
    #' @param documents List of documents with metadata
    #' @param facet_configs List of field names or FacetConfig objects
    #' @return Named list mapping field names to FacetResult
    aggregate = function(documents, facet_configs) {
      # Normalize configs
      configs <- lapply(facet_configs, function(fc) {
        if (is.character(fc)) {
          FacetConfig$new(field = fc)
        } else {
          fc
        }
      })

      results <- list()
      for (config in configs) {
        results[[config$field]] <- private$aggregate_field(documents, config)
      }

      results
    },

    #' @description Convert facet results to list format
    #' @param facet_results Named list of FacetResult objects
    #' @return List format suitable for JSON
    to_list = function(facet_results) {
      lapply(facet_results, function(result) {
        values_map <- list()
        for (fv in result$values) {
          values_map[[as.character(fv$value)]] <- fv$count
        }
        list(
          values = values_map,
          total_count = result$total_count,
          other_count = result$other_count
        )
      })
    }
  ),

  private = list(
    aggregate_field = function(documents, config) {
      counter <- list()

      for (doc in documents) {
        value <- private$get_nested_value(doc, config$field)
        if (is.null(value)) next

        # Handle arrays
        if (is.list(value) || (is.vector(value) && length(value) > 1)) {
          for (v in value) {
            key <- as.character(v)
            counter[[key]] <- (counter[[key]] %||% 0) + 1
          }
        } else {
          key <- as.character(value)
          counter[[key]] <- (counter[[key]] %||% 0) + 1
        }
      }

      # Filter by min_count
      if (!config$include_zero) {
        counter <- counter[sapply(counter, function(x) x >= config$min_count)]
      }

      # Sort
      if (config$sort_by == "count") {
        sorted_names <- names(counter)[order(unlist(counter), decreasing = TRUE)]
      } else {
        sorted_names <- sort(names(counter))
      }

      # Limit
      top_names <- head(sorted_names, config$limit)
      other_names <- tail(sorted_names, length(sorted_names) - config$limit)
      other_count <- sum(unlist(counter[other_names]))

      facet_values <- lapply(top_names, function(name) {
        FacetValue$new(value = name, count = counter[[name]])
      })

      FacetResult$new(
        field = config$field,
        values = facet_values,
        total_count = sum(unlist(counter)),
        other_count = other_count
      )
    },

    get_nested_value = function(doc, field) {
      keys <- strsplit(field, "\\.")[[1]]
      value <- doc

      for (key in keys) {
        if (is.list(value) && key %in% names(value)) {
          value <- value[[key]]
        } else {
          return(NULL)
        }
      }

      value
    }
  )
)

# =============================================================================
# ACL / Security Filtering
# =============================================================================

#' ACL Operator Types
#'
#' @description ACL matching operators
#' @export
ACLOperator <- list(

USER = "user",
  GROUP = "group",
  ROLE = "role",
  EVERYONE = "everyone",
  DENY = "deny"
)

#' ACL Principal
#'
#' @description An ACL principal (user, group, or role)
#' @export
ACLPrincipal <- R6::R6Class(
  "ACLPrincipal",
  public = list(
    #' @field type Principal type
    type = NULL,
    #' @field value Principal value
    value = NULL,

    #' @description Create a new ACLPrincipal
    #' @param type Principal type (user, group, role)
    #' @param value Principal value
    initialize = function(type, value) {
      self$type <- type
      self$value <- value
    },

    #' @description Check if this principal matches another
    #' @param other Another ACLPrincipal
    #' @return Logical
    matches = function(other) {
      if (self$type != other$type) return(FALSE)
      if (self$value == "*" || other$value == "*") return(TRUE)
      self$value == other$value
    },

    #' @description Convert to string
    to_string = function() {
      sprintf("%s:%s", self$type, self$value)
    }
  )
)

#' Parse ACL String
#'
#' @description Parse ACL string like 'user:alice' or 'group:engineering'
#' @param acl_string ACL string
#' @return ACLPrincipal object
#' @export
parse_acl <- function(acl_string) {
  if (grepl(":", acl_string)) {
    parts <- strsplit(acl_string, ":", fixed = TRUE)[[1]]
    type_str <- tolower(parts[1])
    value <- paste(parts[-1], collapse = ":")

    valid_types <- c("user", "group", "role", "everyone", "deny")
    if (type_str %in% valid_types) {
      acl_type <- type_str
    } else {
      acl_type <- "user"
    }
  } else {
    acl_type <- "user"
    value <- acl_string
  }

  ACLPrincipal$new(type = acl_type, value = value)
}

#' ACL Configuration
#'
#' @description ACL configuration for a document or collection
#' @export
ACLConfig <- R6::R6Class(
  "ACLConfig",
  public = list(
    #' @field read_principals Who can read
    read_principals = NULL,
    #' @field deny_principals Who cannot read (takes precedence)
    deny_principals = NULL,
    #' @field is_public Is public access allowed
    is_public = FALSE,

    #' @description Create a new ACLConfig
    #' @param read_principals List of ACLPrincipal objects
    #' @param deny_principals List of ACLPrincipal objects
    #' @param is_public Logical
    initialize = function(read_principals = list(),
                          deny_principals = list(),
                          is_public = FALSE) {
      self$read_principals <- read_principals
      self$deny_principals <- deny_principals
      self$is_public <- is_public
    }
  )
)

#' Create ACL Config from List
#'
#' @description Create ACLConfig from list of ACL strings
#' @param acl_list Character vector of ACL strings
#' @return ACLConfig object
#' @export
acl_config_from_list <- function(acl_list) {
  read <- list()
  deny <- list()
  is_public <- FALSE

  for (acl_str in acl_list) {
    if (tolower(acl_str) %in% c("everyone", "public", "*")) {
      is_public <- TRUE
    } else if (startsWith(acl_str, "deny:")) {
      deny <- c(deny, list(parse_acl(substring(acl_str, 6))))
    } else {
      read <- c(read, list(parse_acl(acl_str)))
    }
  }

  ACLConfig$new(read_principals = read, deny_principals = deny, is_public = is_public)
}

#' ACL Filter
#'
#' @description Access Control List filter for security-aware search
#'
#' @examples
#' \dontrun{
#' acl_filter <- ACLFilter$new()
#' filtered <- acl_filter$filter(
#'   documents = results,
#'   user_principals = c("user:alice", "group:engineering")
#' )
#' }
#'
#' @export
ACLFilter <- R6::R6Class(
  "ACLFilter",
  public = list(
    #' @field acl_field Metadata field containing ACLs
    acl_field = "_acl",

    #' @description Create a new ACLFilter
    #' @param acl_field Field name for ACLs (default: "_acl")
    initialize = function(acl_field = "_acl") {
      self$acl_field <- acl_field
    },

    #' @description Filter documents based on user's ACL principals
    #' @param documents List of documents with metadata
    #' @param user_principals Character vector or list of ACLPrincipal
    #' @param default_allow Allow if no ACL defined (default: FALSE)
    #' @return Filtered documents
    filter = function(documents, user_principals, default_allow = FALSE) {
      # Parse user principals
      principals <- lapply(user_principals, function(p) {
        if (is.character(p)) {
          parse_acl(p)
        } else {
          p
        }
      })

      filtered <- list()
      for (doc in documents) {
        if (private$can_access(doc, principals, default_allow)) {
          filtered <- c(filtered, list(doc))
        }
      }

      filtered
    },

    #' @description Add ACL to document metadata
    #' @param metadata Document metadata
    #' @param principals Character vector of principal strings
    #' @return Updated metadata
    add_acl = function(metadata, principals) {
      metadata[[self$acl_field]] <- principals
      metadata
    },

    #' @description Create ACL filter condition for query
    #' @param user_principals Character vector of principals
    #' @return Filter condition list
    create_filter_condition = function(user_principals) {
      conditions <- list(
        list(field = self$acl_field, op = "$contains", value = "everyone"),
        list(field = self$acl_field, op = "$contains", value = "public")
      )

      for (p in user_principals) {
        conditions <- c(conditions, list(
          list(field = self$acl_field, op = "$contains", value = p)
        ))
      }

      list("$or" = conditions)
    }
  ),

  private = list(
    can_access = function(doc, user_principals, default_allow) {
      metadata <- if (!is.null(doc$metadata)) doc$metadata else doc
      acl_data <- metadata[[self$acl_field]]

      if (is.null(acl_data)) {
        return(default_allow)
      }

      # Parse ACL config
      if (is.character(acl_data)) {
        acl_config <- acl_config_from_list(acl_data)
      } else if (is.list(acl_data)) {
        if (!is.null(acl_data$read) || !is.null(acl_data$deny)) {
          read_principals <- lapply(acl_data$read %||% list(), parse_acl)
          deny_principals <- lapply(acl_data$deny %||% list(), parse_acl)
          acl_config <- ACLConfig$new(
            read_principals = read_principals,
            deny_principals = deny_principals,
            is_public = acl_data$public %||% FALSE
          )
        } else {
          acl_config <- acl_config_from_list(as.character(acl_data))
        }
      } else {
        return(default_allow)
      }

      # Public access
      if (acl_config$is_public) {
        return(TRUE)
      }

      # Check deny list first
      for (deny in acl_config$deny_principals) {
        for (user_p in user_principals) {
          if (deny$matches(user_p)) {
            return(FALSE)
          }
        }
      }

      # Check allow list
      for (allow in acl_config$read_principals) {
        for (user_p in user_principals) {
          if (allow$matches(user_p)) {
            return(TRUE)
          }
        }
      }

      FALSE
    }
  )
)

# =============================================================================
# Text Analyzers
# =============================================================================

#' English Stopwords
#'
#' @description Common English stopwords
#' @export
ENGLISH_STOPWORDS <- c(
  "a", "an", "and", "are", "as", "at", "be", "but", "by", "for",
  "if", "in", "into", "is", "it", "no", "not", "of", "on", "or",
  "such", "that", "the", "their", "then", "there", "these", "they",
  "this", "to", "was", "will", "with", "the", "and", "but", "or",
  "because", "as", "what", "which", "who", "when", "where", "how"
)

#' Simple Stemmer
#'
#' @description Simple suffix-stripping stemmer (no external dependencies)
#' @export
SimpleStemmer <- R6::R6Class(
  "SimpleStemmer",
  public = list(
    #' @field suffixes List of suffixes to remove
    suffixes = c(
      "ization", "ational", "fulness", "ousness", "iveness",
      "ation", "eness", "ment", "ness", "ible", "able", "ity",
      "ing", "ies", "ive", "ion", "ous", "ful", "ism", "ist",
      "ly", "ed", "er", "es", "al", "s"
    ),

    #' @description Stem a word
    #' @param word Word to stem
    #' @return Stemmed word
    stem = function(word) {
      word <- tolower(word)

      for (suffix in self$suffixes) {
        if (endsWith(word, suffix) && nchar(word) - nchar(suffix) >= 3) {
          return(substring(word, 1, nchar(word) - nchar(suffix)))
        }
      }

      word
    },

    #' @description Stem multiple words
    #' @param words Character vector
    #' @return Stemmed words
    stem_words = function(words) {
      sapply(words, self$stem, USE.NAMES = FALSE)
    }
  )
)

#' Text Analyzer
#'
#' @description Text analyzer for search indexing
#'
#' Provides text processing pipelines:
#' - Tokenization
#' - Lowercasing
#' - Stopword removal
#' - Stemming
#' - Synonym expansion
#'
#' @examples
#' \dontrun{
#' analyzer <- TextAnalyzer$english()
#' tokens <- analyzer$analyze("The quick brown foxes are jumping")
#' # c("quick", "brown", "fox", "jump")
#' }
#'
#' @export
TextAnalyzer <- R6::R6Class(
  "TextAnalyzer",
  public = list(
    #' @field lowercase Convert to lowercase
    lowercase = TRUE,
    #' @field remove_stopwords Remove stopwords
    remove_stopwords = FALSE,
    #' @field stopwords Set of stopwords
    stopwords = NULL,
    #' @field stemmer Stemmer object
    stemmer = NULL,
    #' @field synonyms Synonym dictionary
    synonyms = NULL,
    #' @field min_token_length Minimum token length
    min_token_length = 1,
    #' @field max_token_length Maximum token length
    max_token_length = 100,
    #' @field token_pattern Regex pattern for tokens
    token_pattern = "[a-zA-Z0-9]+",

    #' @description Create a new TextAnalyzer
    #' @param lowercase Lowercase text (default: TRUE)
    #' @param remove_stopwords Remove stopwords (default: FALSE)
    #' @param stopwords Custom stopwords (default: ENGLISH_STOPWORDS)
    #' @param use_stemmer Use stemming (default: FALSE)
    #' @param synonyms Named list of synonyms
    #' @param min_token_length Min length (default: 1)
    #' @param max_token_length Max length (default: 100)
    #' @param token_pattern Regex pattern
    initialize = function(lowercase = TRUE,
                          remove_stopwords = FALSE,
                          stopwords = NULL,
                          use_stemmer = FALSE,
                          synonyms = NULL,
                          min_token_length = 1,
                          max_token_length = 100,
                          token_pattern = "[a-zA-Z0-9]+") {
      self$lowercase <- lowercase
      self$remove_stopwords <- remove_stopwords
      self$stopwords <- stopwords %||% ENGLISH_STOPWORDS
      self$synonyms <- synonyms %||% list()
      self$min_token_length <- min_token_length
      self$max_token_length <- max_token_length
      self$token_pattern <- token_pattern

      if (use_stemmer) {
        self$stemmer <- SimpleStemmer$new()
      }
    },

    #' @description Analyze text and return tokens
    #' @param text Input text
    #' @return Character vector of tokens
    analyze = function(text) {
      if (is.null(text) || text == "") {
        return(character(0))
      }

      # Lowercase
      if (self$lowercase) {
        text <- tolower(text)
      }

      # Tokenize
      tokens <- unlist(regmatches(text, gregexpr(self$token_pattern, text)))

      # Filter by length
      tokens <- tokens[nchar(tokens) >= self$min_token_length &
                       nchar(tokens) <= self$max_token_length]

      # Remove stopwords
      if (self$remove_stopwords) {
        tokens <- tokens[!tolower(tokens) %in% self$stopwords]
      }

      # Stem
      if (!is.null(self$stemmer)) {
        tokens <- self$stemmer$stem_words(tokens)
      }

      # Expand synonyms
      if (length(self$synonyms) > 0) {
        expanded <- c()
        for (t in tokens) {
          expanded <- c(expanded, t)
          if (t %in% names(self$synonyms)) {
            expanded <- c(expanded, self$synonyms[[t]])
          }
        }
        tokens <- expanded
      }

      tokens
    },

    #' @description Analyze a query string
    #' @param query Query text
    #' @return Character vector of tokens
    analyze_query = function(query) {
      self$analyze(query)
    }
  )
)

#' Create Standard Text Analyzer
#'
#' @description Lowercase + basic tokenization
#' @return TextAnalyzer object
#' @export
text_analyzer_standard <- function() {
  TextAnalyzer$new(lowercase = TRUE, remove_stopwords = FALSE)
}

#' Create Simple Text Analyzer
#'
#' @description Lowercase + letter-only tokenization
#' @return TextAnalyzer object
#' @export
text_analyzer_simple <- function() {
  TextAnalyzer$new(lowercase = TRUE, token_pattern = "[a-zA-Z]+")
}

#' Create English Text Analyzer
#'
#' @description English analyzer with stemming and stopwords
#' @return TextAnalyzer object
#' @export
text_analyzer_english <- function() {
  TextAnalyzer$new(
    lowercase = TRUE,
    remove_stopwords = TRUE,
    use_stemmer = TRUE,
    min_token_length = 2
  )
}

#' Create Keyword Text Analyzer
#'
#' @description No tokenization - treat input as single token
#' @return TextAnalyzer object
#' @export
text_analyzer_keyword <- function() {
  KeywordAnalyzer$new()
}

#' Keyword Analyzer
#'
#' @description Treats entire input as single token
#' @export
KeywordAnalyzer <- R6::R6Class(
  "KeywordAnalyzer",
  inherit = TextAnalyzer,
  public = list(
    #' @description Analyze text as single keyword
    #' @param text Input text
    #' @return Single-element character vector
    analyze = function(text) {
      if (is.null(text) || text == "") {
        return(character(0))
      }
      c(trimws(tolower(text)))
    }
  )
)

#' Analyzer Chain
#'
#' @description Chain multiple analyzers together
#' @export
AnalyzerChain <- R6::R6Class(
  "AnalyzerChain",
  public = list(
    #' @field analyzers List of TextAnalyzer objects
    analyzers = NULL,

    #' @description Create a new AnalyzerChain
    #' @param analyzers List of TextAnalyzer objects
    initialize = function(analyzers) {
      self$analyzers <- analyzers
    },

    #' @description Run text through all analyzers
    #' @param text Input text
    #' @return Character vector of tokens
    analyze = function(text) {
      tokens <- c(text)

      for (analyzer in self$analyzers) {
        new_tokens <- c()
        for (token in tokens) {
          new_tokens <- c(new_tokens, analyzer$analyze(token))
        }
        tokens <- new_tokens
      }

      tokens
    }
  )
)

# =============================================================================
# Enhanced Search Results
# =============================================================================

#' Enhanced Search Results
#'
#' @description Search results with enterprise features
#' @export
EnhancedSearchResults <- R6::R6Class(
  "EnhancedSearchResults",
  public = list(
    #' @field results List of result items
    results = NULL,
    #' @field facets Named list of FacetResult objects
    facets = NULL,
    #' @field total_count Total results before filtering
    total_count = 0,
    #' @field filtered_count Results after ACL filtering
    filtered_count = 0,
    #' @field query_time_ms Query time in milliseconds
    query_time_ms = 0,
    #' @field rerank_time_ms Rerank time in milliseconds
    rerank_time_ms = 0,
    #' @field facet_time_ms Facet time in milliseconds
    facet_time_ms = 0,

    #' @description Create new EnhancedSearchResults
    #' @param results List of results
    #' @param facets Named list of FacetResult
    #' @param total_count Total count
    #' @param filtered_count Filtered count
    #' @param query_time_ms Query time
    #' @param rerank_time_ms Rerank time
    #' @param facet_time_ms Facet time
    initialize = function(results,
                          facets = list(),
                          total_count = 0,
                          filtered_count = 0,
                          query_time_ms = 0,
                          rerank_time_ms = 0,
                          facet_time_ms = 0) {
      self$results <- results
      self$facets <- facets
      self$total_count <- total_count
      self$filtered_count <- filtered_count
      self$query_time_ms <- query_time_ms
      self$rerank_time_ms <- rerank_time_ms
      self$facet_time_ms <- facet_time_ms
    },

    #' @description Convert to list
    #' @return List representation
    to_list = function() {
      facets_list <- list()
      for (field in names(self$facets)) {
        result <- self$facets[[field]]
        values_map <- list()
        for (fv in result$values) {
          values_map[[as.character(fv$value)]] <- fv$count
        }
        facets_list[[field]] <- list(
          values = values_map,
          total = result$total_count,
          other = result$other_count
        )
      }

      list(
        results = self$results,
        facets = facets_list,
        total_count = self$total_count,
        filtered_count = self$filtered_count,
        query_time_ms = self$query_time_ms,
        rerank_time_ms = self$rerank_time_ms,
        facet_time_ms = self$facet_time_ms
      )
    }
  )
)

#' Null coalescing for advanced_search.R
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a
