# Maximal Marginal Relevance (MMR) Reranker

Reranks for diversity using MMR algorithm

## Public fields

- `lambda`:

  Balance between relevance and diversity (0-1)

## Methods

### Public methods

- [`MMRReranker$new()`](#method-MMRReranker-new)

- [`MMRReranker$rerank()`](#method-MMRReranker-rerank)

- [`MMRReranker$clone()`](#method-MMRReranker-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new MMRReranker

#### Usage

    MMRReranker$new(lambda = 0.7)

#### Arguments

- `lambda`:

  Relevance vs diversity tradeoff (higher = more relevance)

------------------------------------------------------------------------

### Method `rerank()`

Rerank for diversity

#### Usage

    MMRReranker$rerank(query_vector, doc_vectors, doc_ids, scores, limit = 10)

#### Arguments

- `query_vector`:

  Query embedding

- `doc_vectors`:

  Matrix of document embeddings

- `doc_ids`:

  Vector of document IDs

- `scores`:

  Original relevance scores

- `limit`:

  Number of results

#### Returns

Data frame with reranked results

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MMRReranker$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
