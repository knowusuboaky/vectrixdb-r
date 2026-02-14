# Reranker (Cross-Encoder Style Scoring)

Reranks results using term overlap and semantic similarity

## Public fields

- `language`:

  Language setting ("en" or "ml")

## Methods

### Public methods

- [`RerankerEmbedder$new()`](#method-RerankerEmbedder-new)

- [`RerankerEmbedder$score()`](#method-RerankerEmbedder-score)

- [`RerankerEmbedder$clone()`](#method-RerankerEmbedder-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new RerankerEmbedder

#### Usage

    RerankerEmbedder$new(language = "en")

#### Arguments

- `language`:

  Language behavior ("en" = English stopwords, "ml" = Unicode tokens)

------------------------------------------------------------------------

### Method `score()`

Score query-document pairs

#### Usage

    RerankerEmbedder$score(query, documents)

#### Arguments

- `query`:

  Query text

- `documents`:

  Character vector of document texts

#### Returns

Numeric vector of scores (0-1)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    RerankerEmbedder$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
