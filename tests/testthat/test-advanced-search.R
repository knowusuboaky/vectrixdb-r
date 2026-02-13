# Tests for advanced_search.R - Facets, ACL, Text Analyzers

test_that("FacetConfig works correctly", {
  config <- FacetConfig$new(
    field = "category",
    limit = 5,
    min_count = 2,
    sort_by = "count"
  )

  expect_equal(config$field, "category")
  expect_equal(config$limit, 5)
  expect_equal(config$min_count, 2)
  expect_equal(config$sort_by, "count")
  expect_false(config$include_zero)
})

test_that("FacetAggregator aggregates correctly", {
  aggregator <- FacetAggregator$new()

  documents <- list(
    list(category = "tech", author = "Alice"),
    list(category = "tech", author = "Bob"),
    list(category = "science", author = "Alice"),
    list(category = "tech", author = "Charlie"),
    list(category = "health", author = "Alice")
  )

  facets <- aggregator$aggregate(documents, c("category", "author"))

  # Category facet
  expect_true("category" %in% names(facets))
  category_facet <- facets$category
  expect_equal(category_facet$total_count, 5)

  # Find tech count
  tech_count <- 0
  for (fv in category_facet$values) {
    if (fv$value == "tech") tech_count <- fv$count
  }
  expect_equal(tech_count, 3)

  # Author facet
  expect_true("author" %in% names(facets))
  author_facet <- facets$author
  alice_count <- 0
  for (fv in author_facet$values) {
    if (fv$value == "Alice") alice_count <- fv$count
  }
  expect_equal(alice_count, 3)
})

test_that("FacetAggregator handles nested fields", {
  aggregator <- FacetAggregator$new()

  documents <- list(
    list(meta = list(type = "A")),
    list(meta = list(type = "A")),
    list(meta = list(type = "B"))
  )

  facets <- aggregator$aggregate(documents, c("meta.type"))

  expect_true("meta.type" %in% names(facets))
  expect_equal(facets$`meta.type`$total_count, 3)
})

test_that("ACLPrincipal parsing works", {
  # User principal
  p1 <- parse_acl("user:alice")
  expect_equal(p1$type, "user")
  expect_equal(p1$value, "alice")

  # Group principal
  p2 <- parse_acl("group:engineering")
  expect_equal(p2$type, "group")
  expect_equal(p2$value, "engineering")

  # Role principal
  p3 <- parse_acl("role:admin")
  expect_equal(p3$type, "role")
  expect_equal(p3$value, "admin")

  # Default to user
  p4 <- parse_acl("bob")
  expect_equal(p4$type, "user")
  expect_equal(p4$value, "bob")
})

test_that("ACLPrincipal matching works", {
  p1 <- ACLPrincipal$new("user", "alice")
  p2 <- ACLPrincipal$new("user", "alice")
  p3 <- ACLPrincipal$new("user", "bob")
  p4 <- ACLPrincipal$new("group", "alice")

  # Same principal

  expect_true(p1$matches(p2))

  # Different value
  expect_false(p1$matches(p3))

  # Different type
  expect_false(p1$matches(p4))

  # Wildcard
  p_wild <- ACLPrincipal$new("user", "*")
  expect_true(p_wild$matches(p1))
})

test_that("ACLConfig from list works", {
  config <- acl_config_from_list(c("user:alice", "group:engineering", "public"))

  expect_true(config$is_public)
  expect_equal(length(config$read_principals), 2)

  # With deny
  config2 <- acl_config_from_list(c("user:alice", "deny:user:bob"))
  expect_equal(length(config2$deny_principals), 1)
  expect_equal(config2$deny_principals[[1]]$value, "bob")
})

test_that("ACLFilter filters documents correctly", {
  filter <- ACLFilter$new()

  documents <- list(
    list(id = "doc1", metadata = list(`_acl` = c("user:alice", "group:engineering"))),
    list(id = "doc2", metadata = list(`_acl` = c("user:bob"))),
    list(id = "doc3", metadata = list(`_acl` = c("public"))),
    list(id = "doc4", metadata = list())  # No ACL
  )

  # Alice can see doc1 and doc3
  filtered <- filter$filter(documents, c("user:alice"), default_allow = FALSE)
  ids <- sapply(filtered, function(d) d$id)
  expect_true("doc1" %in% ids)
  expect_true("doc3" %in% ids)
  expect_false("doc2" %in% ids)

  # With default_allow, doc4 is included
  filtered2 <- filter$filter(documents, c("user:alice"), default_allow = TRUE)
  ids2 <- sapply(filtered2, function(d) d$id)
  expect_true("doc4" %in% ids2)
})

test_that("SimpleStemmer works correctly", {
  stemmer <- SimpleStemmer$new()

  expect_equal(stemmer$stem("running"), "runn")
  expect_equal(stemmer$stem("happiness"), "happi")
  expect_equal(stemmer$stem("beautiful"), "beauti")
  expect_equal(stemmer$stem("quickly"), "quick")

  # Short words unchanged
  expect_equal(stemmer$stem("go"), "go")
})

test_that("TextAnalyzer tokenizes correctly", {
  analyzer <- TextAnalyzer$new(
    lowercase = TRUE,
    remove_stopwords = FALSE
  )

  tokens <- analyzer$analyze("Hello World 123")
  expect_equal(tokens, c("hello", "world", "123"))
})

test_that("TextAnalyzer removes stopwords", {
  analyzer <- TextAnalyzer$new(
    lowercase = TRUE,
    remove_stopwords = TRUE
  )

  tokens <- analyzer$analyze("The quick brown fox is fast")
  expect_false("the" %in% tokens)
  expect_false("is" %in% tokens)
  expect_true("quick" %in% tokens)
  expect_true("brown" %in% tokens)
})

test_that("TextAnalyzer applies stemming", {
  analyzer <- TextAnalyzer$new(
    lowercase = TRUE,
    remove_stopwords = TRUE,
    use_stemmer = TRUE
  )

  tokens <- analyzer$analyze("running jumps quickly")
  # Stemmed versions
  expect_true("runn" %in% tokens || "running" %in% tokens)
})

test_that("text_analyzer_english works", {
  analyzer <- text_analyzer_english()

  tokens <- analyzer$analyze("The quick brown foxes are jumping")
  expect_false("the" %in% tokens)
  expect_false("are" %in% tokens)
  expect_true(length(tokens) > 0)
})

test_that("KeywordAnalyzer treats input as single token", {
  analyzer <- text_analyzer_keyword()

  tokens <- analyzer$analyze("Hello World")
  expect_equal(length(tokens), 1)
  expect_equal(tokens[1], "hello world")
})

test_that("AnalyzerChain chains analyzers", {
  analyzer1 <- TextAnalyzer$new(lowercase = TRUE)
  analyzer2 <- TextAnalyzer$new(remove_stopwords = TRUE)

  chain <- AnalyzerChain$new(list(analyzer1, analyzer2))

  tokens <- chain$analyze("THE Quick")
  expect_true("quick" %in% tokens)
})

test_that("EnhancedSearchResults works correctly", {
  results <- EnhancedSearchResults$new(
    results = list(list(id = "1", score = 0.9)),
    facets = list(),
    total_count = 100,
    filtered_count = 50,
    query_time_ms = 15.5
  )

  expect_equal(results$total_count, 100)
  expect_equal(results$filtered_count, 50)
  expect_equal(results$query_time_ms, 15.5)

  result_list <- results$to_list()
  expect_true(is.list(result_list))
  expect_equal(result_list$total_count, 100)
})
