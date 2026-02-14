# GraphRAG Configuration

Configuration for VectrixDB's GraphRAG implementation

## Public fields

- `enabled`:

  Whether GraphRAG is enabled

- `chunk_size`:

  Target tokens per chunk

- `chunk_overlap`:

  Overlapping tokens

- `chunk_by_sentence`:

  Preserve sentence boundaries

- `extractor`:

  Extraction method

- `nlp_model`:

  NLP model name

- `llm_provider`:

  LLM provider

- `llm_model`:

  Model name

- `llm_api_key`:

  API key

- `llm_endpoint`:

  Custom endpoint

- `llm_temperature`:

  Temperature

- `llm_max_tokens`:

  Max tokens

- `max_community_levels`:

  Max hierarchy depth

- `min_community_size`:

  Min entities per community

- `relationship_threshold`:

  Min relationship strength

- `deduplicate_entities`:

  Merge similar entities

- `entity_similarity_threshold`:

  Similarity for dedup

- `search_type`:

  Default search strategy

- `local_search_k`:

  Seed entities for local search

- `global_search_k`:

  Communities for global search

- `traversal_depth`:

  Max hops

- `include_relationships`:

  Include relationship context

- `include_community_context`:

  Include community summaries

- `enable_incremental`:

  Incremental updates

- `batch_size`:

  Chunks per batch

- `use_cache`:

  Cache embeddings

- `cache_ttl`:

  Cache TTL seconds

- `entity_types`:

  Types to extract

- `relationship_types`:

  Types to extract

## Methods

### Public methods

- [`GraphRAGConfig$new()`](#method-GraphRAGConfig-new)

- [`GraphRAGConfig$with_openai()`](#method-GraphRAGConfig-with_openai)

- [`GraphRAGConfig$with_ollama()`](#method-GraphRAGConfig-with_ollama)

- [`GraphRAGConfig$clone()`](#method-GraphRAGConfig-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new GraphRAGConfig

#### Usage

    GraphRAGConfig$new(enabled = FALSE, ...)

#### Arguments

- `enabled`:

  Enable GraphRAG

- `...`:

  Additional configuration options

------------------------------------------------------------------------

### Method `with_openai()`

Configure for OpenAI

#### Usage

    GraphRAGConfig$with_openai(model = "gpt-4o-mini", api_key = NULL)

#### Arguments

- `model`:

  Model name

- `api_key`:

  API key

#### Returns

Self

------------------------------------------------------------------------

### Method `with_ollama()`

Configure for Ollama

#### Usage

    GraphRAGConfig$with_ollama(
      model = "llama3.2",
      endpoint = "http://localhost:11434"
    )

#### Arguments

- `model`:

  Model name

- `endpoint`:

  Endpoint URL

#### Returns

Self

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    GraphRAGConfig$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
config <- GraphRAGConfig$new(enabled = TRUE)
db <- Vectrix$new("knowledge_base", graphrag_config = config)
} # }
```
