# Advanced Reranker with Learned Weights

Combines multiple signals for better reranking:

- Semantic similarity (word vectors)

- BM25/keyword overlap

- Query coverage

- Position bias

- Length normalization

## Public fields

- `weights`:

  Feature weights

## Methods

### Public methods

- [`AdvancedReranker$new()`](#method-AdvancedReranker-new)

- [`AdvancedReranker$set_embedder()`](#method-AdvancedReranker-set_embedder)

- [`AdvancedReranker$rerank()`](#method-AdvancedReranker-rerank)

- [`AdvancedReranker$learn_weights()`](#method-AdvancedReranker-learn_weights)

- [`AdvancedReranker$clone()`](#method-AdvancedReranker-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new AdvancedReranker

#### Usage

    AdvancedReranker$new(
      semantic_weight = 0.4,
      bm25_weight = 0.3,
      coverage_weight = 0.2,
      position_weight = 0.1,
      sentence_embedder = NULL
    )

#### Arguments

- `semantic_weight`:

  Weight for semantic similarity (0-1)

- `bm25_weight`:

  Weight for BM25 score (0-1)

- `coverage_weight`:

  Weight for query term coverage (0-1)

- `position_weight`:

  Weight for position bias (0-1)

- `sentence_embedder`:

  Optional SentenceEmbedder for semantic scoring

------------------------------------------------------------------------

### Method `set_embedder()`

Set sentence embedder

#### Usage

    AdvancedReranker$set_embedder(embedder)

#### Arguments

- `embedder`:

  SentenceEmbedder object

------------------------------------------------------------------------

### Method `rerank()`

Rerank results

#### Usage

    AdvancedReranker$rerank(
      query,
      query_vector = NULL,
      results,
      doc_vectors = NULL,
      limit = 10
    )

#### Arguments

- `query`:

  Query text

- `query_vector`:

  Query embedding vector

- `results`:

  List of result objects with id, text, score

- `doc_vectors`:

  Matrix of document vectors (optional)

- `limit`:

  Number of results to return

#### Returns

Reranked list of results

------------------------------------------------------------------------

### Method `learn_weights()`

Learn optimal weights from relevance judgments

#### Usage

    AdvancedReranker$learn_weights(
      queries,
      results_list,
      relevance_list,
      iterations = 100
    )

#### Arguments

- `queries`:

  Character vector of queries

- `results_list`:

  List of result lists (one per query)

- `relevance_list`:

  List of relevance scores (1=relevant, 0=not)

- `iterations`:

  Number of optimization iterations

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    AdvancedReranker$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
