# Load word vectors into memory

Loads pre-trained word vectors from a file

## Usage

``` r
load_word_vectors(path, max_words = NULL, normalize = TRUE)
```

## Arguments

- path:

  Path to word vectors file (GloVe .txt or word2vec .bin)

- max_words:

  Maximum number of words to load (NULL for all)

- normalize:

  Normalize vectors to unit length

## Value

WordVectors object
