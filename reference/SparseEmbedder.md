# Sparse Embedder (BM25/TF-IDF)

Generates sparse BM25 embeddings for keyword search

## Public fields

- `vocab`:

  Vocabulary

- `language`:

  Language setting ("en" or "ml")

## Methods

### Public methods

- [`SparseEmbedder$new()`](#method-SparseEmbedder-new)

- [`SparseEmbedder$fit()`](#method-SparseEmbedder-fit)

- [`SparseEmbedder$embed()`](#method-SparseEmbedder-embed)

- [`SparseEmbedder$query_terms()`](#method-SparseEmbedder-query_terms)

- [`SparseEmbedder$clone()`](#method-SparseEmbedder-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new SparseEmbedder

#### Usage

    SparseEmbedder$new(language = "en")

#### Arguments

- `language`:

  Language behavior ("en" = ASCII-focused, "ml" = Unicode-aware)

------------------------------------------------------------------------

### Method `fit()`

Fit the embedder on a corpus

#### Usage

    SparseEmbedder$fit(texts)

#### Arguments

- `texts`:

  Character vector of texts

------------------------------------------------------------------------

### Method [`embed()`](https://rdrr.io/r/stats/embed.html)

Embed texts to sparse vectors

#### Usage

    SparseEmbedder$embed(texts)

#### Arguments

- `texts`:

  Character vector of texts

#### Returns

Sparse matrix of BM25 scores

------------------------------------------------------------------------

### Method `query_terms()`

Get term scores for a query

#### Usage

    SparseEmbedder$query_terms(query)

#### Arguments

- `query`:

  Query text

#### Returns

Named vector of term scores

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SparseEmbedder$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
