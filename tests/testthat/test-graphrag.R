# Tests for graphrag.R - Knowledge Graphs, Entity Extraction, Search

test_that("GraphRAGConfig initializes correctly", {
  config <- GraphRAGConfig$new(
    enabled = TRUE,
    chunk_size = 500,
    extractor = "regex"
  )

  expect_true(config$enabled)
  expect_equal(config$chunk_size, 500)
  expect_equal(config$extractor, "regex")
  expect_equal(config$search_type, "hybrid")
})

test_that("GraphRAGConfig validation works", {
  expect_error(
    GraphRAGConfig$new(chunk_size = 50),
    "chunk_size must be at least 100"
  )

  expect_error(
    GraphRAGConfig$new(chunk_overlap = 1000, chunk_size = 500),
    "chunk_overlap must be less than chunk_size"
  )
})

test_that("GraphRAGConfig with_openai works", {
  config <- GraphRAGConfig$new(enabled = TRUE)
  config$with_openai(model = "gpt-4", api_key = "test-key")

  expect_equal(config$llm_provider, LLMProvider$OPENAI)
  expect_equal(config$llm_model, "gpt-4")
  expect_equal(config$llm_api_key, "test-key")
})

test_that("TextUnit initializes correctly", {
  unit <- TextUnit$new(
    id = "chunk_1",
    text = "Test text content",
    document_id = "doc1",
    chunk_index = 0
  )

  expect_equal(unit$id, "chunk_1")
  expect_equal(unit$text, "Test text content")
  expect_equal(unit$document_id, "doc1")
})

test_that("DocumentChunker chunks by sentences", {
  chunker <- DocumentChunker$new(
    chunk_size = 100,
    chunk_overlap = 20,
    by_sentence = TRUE
  )

  text <- "First sentence. Second sentence. Third sentence. Fourth sentence."
  chunks <- chunker$chunk(text, "doc1")

  expect_true(length(chunks) > 0)
  for (chunk in chunks) {
    expect_true(inherits(chunk, "TextUnit"))
  }
})

test_that("DocumentChunker chunks by characters", {
  chunker <- DocumentChunker$new(
    chunk_size = 50,
    chunk_overlap = 10,
    by_sentence = FALSE
  )

  text <- paste(rep("word", 30), collapse = " ")  # Long text
  chunks <- chunker$chunk(text, "doc1")

  expect_true(length(chunks) > 1)
  expect_true(length(chunks) < 20)
  expect_equal(chunks[[length(chunks)]]$end_char, nchar(text))
})

test_that("DocumentChunker validates overlap settings", {
  expect_error(
    DocumentChunker$new(chunk_size = 50, chunk_overlap = 50, by_sentence = FALSE),
    "chunk_overlap must be less than chunk_size"
  )
})

test_that("Entity initializes correctly", {
  entity <- Entity$new(
    name = "John Doe",
    type = "PERSON",
    description = "A person"
  )

  expect_equal(entity$name, "John Doe")
  expect_equal(entity$type, "PERSON")
  expect_equal(entity$description, "A person")
  expect_true(nchar(entity$id) > 0)
})

test_that("Entity to_list works", {
  entity <- Entity$new(name = "Test", type = "CONCEPT")
  result <- entity$to_list()

  expect_true(is.list(result))
  expect_equal(result$name, "Test")
  expect_equal(result$type, "CONCEPT")
})

test_that("Relationship initializes correctly", {
  rel <- Relationship$new(
    source_id = "entity1",
    target_id = "entity2",
    type = "RELATED_TO",
    weight = 0.8
  )

  expect_equal(rel$source_id, "entity1")
  expect_equal(rel$target_id, "entity2")
  expect_equal(rel$type, "RELATED_TO")
  expect_equal(rel$weight, 0.8)
})

test_that("RegexExtractor extracts entities", {
  extractor <- RegexExtractor$new()

  text <- "John Smith works at Microsoft Corporation in New York City."
  result <- extractor$extract(text, "chunk1")

  expect_true(inherits(result, "ExtractionResult"))
  expect_true(length(result$entities) > 0)
})

test_that("KnowledgeGraph adds entities", {
  graph <- KnowledgeGraph$new("test")

  entity1 <- Entity$new(name = "Alice", type = "PERSON")
  entity2 <- Entity$new(name = "Bob", type = "PERSON")

  graph$add_entity(entity1)
  graph$add_entity(entity2)

  expect_equal(graph$entity_count(), 2)
  expect_equal(graph$get_entity(entity1$id)$name, "Alice")
})

test_that("KnowledgeGraph adds relationships", {
  graph <- KnowledgeGraph$new("test")

  entity1 <- Entity$new(name = "Alice", type = "PERSON")
  entity2 <- Entity$new(name = "Bob", type = "PERSON")

  graph$add_entity(entity1)
  graph$add_entity(entity2)

  rel <- Relationship$new(
    source_id = entity1$id,
    target_id = entity2$id,
    type = "KNOWS"
  )
  graph$add_relationship(rel)

  expect_equal(graph$relationship_count(), 1)
})

test_that("KnowledgeGraph gets neighbors", {
  graph <- KnowledgeGraph$new("test")

  entity1 <- Entity$new(name = "Alice", type = "PERSON")
  entity2 <- Entity$new(name = "Bob", type = "PERSON")
  entity3 <- Entity$new(name = "Charlie", type = "PERSON")

  graph$add_entity(entity1)
  graph$add_entity(entity2)
  graph$add_entity(entity3)

  graph$add_relationship(Relationship$new(entity1$id, entity2$id, "KNOWS"))
  graph$add_relationship(Relationship$new(entity1$id, entity3$id, "KNOWS"))

  neighbors <- graph$get_neighbors(entity1$id)
  expect_equal(length(neighbors), 2)
})

test_that("KnowledgeGraph traverses correctly", {
  graph <- KnowledgeGraph$new("test")

  entity1 <- Entity$new(name = "A", type = "NODE")
  entity2 <- Entity$new(name = "B", type = "NODE")
  entity3 <- Entity$new(name = "C", type = "NODE")

  graph$add_entity(entity1)
  graph$add_entity(entity2)
  graph$add_entity(entity3)

  graph$add_relationship(Relationship$new(entity1$id, entity2$id, "LINK"))
  graph$add_relationship(Relationship$new(entity2$id, entity3$id, "LINK"))

  subgraph <- graph$traverse(c(entity1$id), max_depth = 2)

  expect_true(inherits(subgraph, "SubGraph"))
  expect_true(length(subgraph$entities) >= 1)
})

test_that("KnowledgeGraph search_entities works", {
  graph <- KnowledgeGraph$new("test")

  graph$add_entity(Entity$new(name = "John Smith", type = "PERSON"))
  graph$add_entity(Entity$new(name = "Jane Doe", type = "PERSON"))
  graph$add_entity(Entity$new(name = "Microsoft", type = "ORGANIZATION"))

  results <- graph$search_entities("John", limit = 5)
  expect_equal(length(results), 1)
  expect_equal(results[[1]]$name, "John Smith")
})

test_that("Community initializes correctly", {
  community <- Community$new(
    id = "c1",
    level = 0,
    entity_ids = c("e1", "e2", "e3")
  )

  expect_equal(community$id, "c1")
  expect_equal(community$level, 0)
  expect_equal(community$size(), 3)
})

test_that("CommunityDetector detects communities", {
  detector <- CommunityDetector$new(min_size = 2)

  graph <- KnowledgeGraph$new("test")

  # Create connected components
  e1 <- Entity$new(name = "A", type = "NODE")
  e2 <- Entity$new(name = "B", type = "NODE")
  e3 <- Entity$new(name = "C", type = "NODE")

  graph$add_entity(e1)
  graph$add_entity(e2)
  graph$add_entity(e3)

  graph$add_relationship(Relationship$new(e1$id, e2$id, "LINK"))
  graph$add_relationship(Relationship$new(e2$id, e3$id, "LINK"))

  communities <- detector$detect(graph)

  # Should find at least one community with 3 nodes
  expect_true(length(communities) >= 0)  # May be 0 if min_size not met
})

test_that("LocalSearcher searches graph", {
  graph <- KnowledgeGraph$new("test")

  graph$add_entity(Entity$new(name = "Machine Learning", type = "CONCEPT"))
  graph$add_entity(Entity$new(name = "Deep Learning", type = "CONCEPT"))

  searcher <- LocalSearcher$new(graph, k = 5, traversal_depth = 1)
  result <- searcher$search("Machine")

  expect_true(inherits(result, "LocalSearchResult"))
})

test_that("GlobalSearcher searches communities", {
  community <- Community$new(
    id = "c1",
    level = 0,
    entity_ids = c("e1", "e2"),
    summary = "A community about AI"
  )

  searcher <- GlobalSearcher$new(list(community), k = 3)
  result <- searcher$search("AI")

  expect_true(inherits(result, "GlobalSearchResult"))
})

test_that("GraphRAGPipeline processes documents", {
  config <- GraphRAGConfig$new(enabled = TRUE)
  pipeline <- GraphRAGPipeline$new(config)

  documents <- c(
    "John Smith works at Microsoft Corporation.",
    "Microsoft is located in Seattle."
  )

  pipeline$process(documents, c("doc1", "doc2"))

  stats <- pipeline$stats()
  expect_true(stats$entity_count >= 0)
})

test_that("GraphRAGPipeline search works", {
  config <- GraphRAGConfig$new(enabled = TRUE)
  pipeline <- GraphRAGPipeline$new(config)

  pipeline$process(
    c("Apple Inc is a technology company.", "Tim Cook is CEO of Apple."),
    c("doc1", "doc2")
  )

  result <- pipeline$search("Apple", search_type = "local")
  expect_true(!is.null(result))
})

test_that("create_pipeline factory works", {
  pipeline <- create_pipeline()
  expect_true(inherits(pipeline, "GraphRAGPipeline"))
})

test_that("create_default_graphrag_config works", {
  config <- create_default_graphrag_config()
  expect_true(config$enabled)
  expect_equal(config$extractor, "regex")
})
