# Create a sentence embedder with automatic download

Convenience function to create a SentenceEmbedder with GloVe vectors

## Usage

``` r
create_sentence_embedder(model = "glove-100", use_idf = TRUE)
```

## Arguments

- model:

  Model name (default: "glove-100")

- use_idf:

  Use IDF weighting

## Value

SentenceEmbedder object

## Examples

``` r
if (FALSE) { # \dontrun{
# Downloads GloVe if not present
embedder <- create_sentence_embedder("glove-100")

# Embed texts
vectors <- embedder$embed(c("Hello world", "Machine learning is cool"))
} # }
```
