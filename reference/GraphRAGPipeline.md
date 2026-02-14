# GraphRAG Pipeline

Complete GraphRAG processing pipeline

## Public fields

- `config`:

  GraphRAGConfig

- `graph`:

  KnowledgeGraph

- `communities`:

  Detected communities

## Methods

### Public methods

- [`GraphRAGPipeline$new()`](#method-GraphRAGPipeline-new)

- [`GraphRAGPipeline$process()`](#method-GraphRAGPipeline-process)

- [`GraphRAGPipeline$search()`](#method-GraphRAGPipeline-search)

- [`GraphRAGPipeline$stats()`](#method-GraphRAGPipeline-stats)

- [`GraphRAGPipeline$clone()`](#method-GraphRAGPipeline-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new GraphRAGPipeline

#### Usage

    GraphRAGPipeline$new(config = NULL)

#### Arguments

- `config`:

  GraphRAGConfig

------------------------------------------------------------------------

### Method `process()`

Process documents

#### Usage

    GraphRAGPipeline$process(texts, document_ids = NULL)

#### Arguments

- `texts`:

  Character vector of documents

- `document_ids`:

  Document IDs

#### Returns

Self

------------------------------------------------------------------------

### Method [`search()`](https://rdrr.io/r/base/search.html)

Search the graph

#### Usage

    GraphRAGPipeline$search(query, search_type = NULL)

#### Arguments

- `query`:

  Query string

- `search_type`:

  "local", "global", or "hybrid"

#### Returns

Search result

------------------------------------------------------------------------

### Method `stats()`

Get statistics

#### Usage

    GraphRAGPipeline$stats()

#### Returns

Named list

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    GraphRAGPipeline$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
