# Late Interaction Embedder (Simplified ColBERT-style)

Token-level embeddings for late interaction scoring

## Public fields

- `dimension`:

  Token embedding dimension

- `language`:

  Language setting ("en" or "ml")

## Methods

### Public methods

- [`LateInteractionEmbedder$new()`](#method-LateInteractionEmbedder-new)

- [`LateInteractionEmbedder$embed()`](#method-LateInteractionEmbedder-embed)

- [`LateInteractionEmbedder$score()`](#method-LateInteractionEmbedder-score)

- [`LateInteractionEmbedder$clone()`](#method-LateInteractionEmbedder-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new LateInteractionEmbedder

#### Usage

    LateInteractionEmbedder$new(dimension = 64, language = "en")

#### Arguments

- `dimension`:

  Embedding dimension per token

- `language`:

  Language behavior ("en" = ASCII-focused, "ml" = Unicode-aware)

------------------------------------------------------------------------

### Method [`embed()`](https://rdrr.io/r/stats/embed.html)

Embed texts to token-level embeddings

#### Usage

    LateInteractionEmbedder$embed(texts)

#### Arguments

- `texts`:

  Character vector of texts

#### Returns

List of matrices (each matrix is token embeddings for a document)

------------------------------------------------------------------------

### Method `score()`

Compute late interaction (MaxSim) score

#### Usage

    LateInteractionEmbedder$score(query_embeddings, doc_embeddings)

#### Arguments

- `query_embeddings`:

  Query token embeddings matrix

- `doc_embeddings`:

  Document token embeddings matrix

#### Returns

Numeric score

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LateInteractionEmbedder$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
