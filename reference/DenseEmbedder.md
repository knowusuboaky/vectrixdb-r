# Dense Embedder using word2vec or GloVe

Generates dense vector embeddings using pre-trained word vectors

## Public fields

- `dimension`:

  Embedding dimension

- `model_type`:

  Type of model being used

- `language`:

  Language setting ("en" or "ml")

## Methods

### Public methods

- [`DenseEmbedder$new()`](#method-DenseEmbedder-new)

- [`DenseEmbedder$set_sentence_embedder()`](#method-DenseEmbedder-set_sentence_embedder)

- [`DenseEmbedder$embed()`](#method-DenseEmbedder-embed)

- [`DenseEmbedder$fit()`](#method-DenseEmbedder-fit)

- [`DenseEmbedder$clone()`](#method-DenseEmbedder-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new DenseEmbedder

#### Usage

    DenseEmbedder$new(
      dimension = 100,
      model_path = NULL,
      model_type = "tfidf",
      sentence_embedder = NULL,
      auto_download = FALSE,
      language = "en"
    )

#### Arguments

- `dimension`:

  Vector dimension (default: 100 for word2vec, 50/100/200/300 for GloVe)

- `model_path`:

  Optional path to pre-trained model file

- `model_type`:

  Type: "word2vec", "glove", "glove-pretrained", or "tfidf"

- `sentence_embedder`:

  Optional SentenceEmbedder object to use

- `auto_download`:

  Auto-download GloVe vectors if model_type is glove-pretrained

- `language`:

  Language behavior ("en" = ASCII-focused, "ml" = Unicode-aware)

------------------------------------------------------------------------

### Method `set_sentence_embedder()`

Set a SentenceEmbedder to use for embeddings

#### Usage

    DenseEmbedder$set_sentence_embedder(embedder)

#### Arguments

- `embedder`:

  SentenceEmbedder object

------------------------------------------------------------------------

### Method [`embed()`](https://rdrr.io/r/stats/embed.html)

Embed texts to vectors

#### Usage

    DenseEmbedder$embed(texts)

#### Arguments

- `texts`:

  Character vector of texts

#### Returns

Matrix of embeddings (rows are documents)

------------------------------------------------------------------------

### Method `fit()`

Train embedder on corpus (for TF-IDF)

#### Usage

    DenseEmbedder$fit(texts)

#### Arguments

- `texts`:

  Character vector of training texts

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DenseEmbedder$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
