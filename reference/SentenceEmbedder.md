# Sentence Embedder using Word Vectors

Creates sentence embeddings by averaging word vectors with IDF weighting

## Public fields

- `dim`:

  Embedding dimension

- `vocab_size`:

  Vocabulary size

## Methods

### Public methods

- [`SentenceEmbedder$new()`](#method-SentenceEmbedder-new)

- [`SentenceEmbedder$fit()`](#method-SentenceEmbedder-fit)

- [`SentenceEmbedder$embed()`](#method-SentenceEmbedder-embed)

- [`SentenceEmbedder$get_word_vector()`](#method-SentenceEmbedder-get_word_vector)

- [`SentenceEmbedder$has_word()`](#method-SentenceEmbedder-has_word)

- [`SentenceEmbedder$most_similar()`](#method-SentenceEmbedder-most_similar)

- [`SentenceEmbedder$clone()`](#method-SentenceEmbedder-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new SentenceEmbedder

#### Usage

    SentenceEmbedder$new(word_vectors, use_idf = TRUE, smooth_idf = 1)

#### Arguments

- `word_vectors`:

  WordVectors object from load_word_vectors()

- `use_idf`:

  Use IDF weighting (recommended)

- `smooth_idf`:

  Smoothing for IDF

------------------------------------------------------------------------

### Method `fit()`

Fit IDF weights on a corpus

#### Usage

    SentenceEmbedder$fit(texts)

#### Arguments

- `texts`:

  Character vector of texts

------------------------------------------------------------------------

### Method [`embed()`](https://rdrr.io/r/stats/embed.html)

Embed texts to sentence vectors

#### Usage

    SentenceEmbedder$embed(texts)

#### Arguments

- `texts`:

  Character vector of texts

#### Returns

Matrix of embeddings (rows are sentences)

------------------------------------------------------------------------

### Method `get_word_vector()`

Get word vector for a single word

#### Usage

    SentenceEmbedder$get_word_vector(word)

#### Arguments

- `word`:

  Word to look up

#### Returns

Numeric vector or NULL if not found

------------------------------------------------------------------------

### Method `has_word()`

Check if word is in vocabulary

#### Usage

    SentenceEmbedder$has_word(word)

#### Arguments

- `word`:

  Word to check

#### Returns

Logical

------------------------------------------------------------------------

### Method `most_similar()`

Find most similar words

#### Usage

    SentenceEmbedder$most_similar(word, n = 10)

#### Arguments

- `word`:

  Query word

- `n`:

  Number of results

#### Returns

Data frame with word and similarity

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SentenceEmbedder$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
