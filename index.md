# VectrixDB

## Overview

**Zero config. Text in, results out. Pure R - no Python required.**

VectrixDB is a lightweight, pure R vector database with built-in text
embeddings. No external dependencies, no API keys, no Python - just
install and use.

## Installation

``` r
# Install from GitHub
devtools::install_github("knowusuboaky/vectrixdb-r")

# Required dependencies
install.packages(c("R6", "text2vec", "digest", "Matrix"))

# Optional for better performance
install.packages(c("RcppAnnoy", "word2vec", "stopwords"))
```

## Quick Start

``` r
library(VectrixDB)

# Create and add - ONE LINE
db <- Vectrix$new("my_docs")$add(c("Python is great", "Machine learning is fun", "R is awesome"))

# Search - ONE LINE
results <- db$search("programming")

# Get top result
print(results$top()$text)
#> "R is awesome"
```

## Features

### Pure R - No Python Required

Unlike other vector databases, VectrixDB is 100% R: - **TF-IDF
embeddings** - Built-in, works out of the box - **BM25 search** - Using
`text2vec` - **Optional word vectors** - Load GloVe or word2vec files -
**ANN indexing** - Fast search with `RcppAnnoy`

### Search Modes

``` r
# Dense (semantic) search using TF-IDF/word vectors
results <- db$search("query", mode = "dense")

# Sparse (keyword/BM25) search
results <- db$search("query", mode = "sparse")

# Hybrid (dense + sparse with RRF fusion)
results <- db$search("query", mode = "hybrid")

# Ultimate (hybrid + reranking)
results <- db$search("query", mode = "ultimate")
```

### Embedding Options

``` r
# Default: TF-IDF (no external files needed)
db <- Vectrix$new("docs")

# With GloVe word vectors (download from Stanford NLP)
db <- Vectrix$new("docs", model = "glove", model_path = "glove.6B.100d.txt")

# With word2vec (download or train your own)
db <- Vectrix$new("docs", model = "word2vec", model_path = "GoogleNews-vectors.bin")

# Custom embedding function
my_embed <- function(texts) {
  # Your custom logic
  matrix(rnorm(length(texts) * 100), nrow = length(texts))
}
db <- Vectrix$new("docs", embed_fn = my_embed, dimension = 100)
```

### Metadata Filtering

``` r
# Add with metadata
db$add(
  texts = c("Python guide", "ML tutorial", "R handbook"),
  metadata = list(
    list(category = "programming", year = 2024),
    list(category = "ai", year = 2024),
    list(category = "programming", year = 2023)
  )
)

# Search with filter
results <- db$search("guide", filter = list(category = "programming"))
```

### Reranking

``` r
# MMR for diversity
results <- db$search("AI", rerank = "mmr", diversity = 0.7)

# Cross-encoder style reranking
results <- db$search("AI", rerank = "cross-encoder")
```

### Results API

``` r
results <- db$search("query")

# Access results
results$top()          # Get top result
results$texts()        # All result texts as vector
results$ids()          # All result IDs
results$scores()       # All scores
results$length()       # Number of results
results$get(2)         # Get second result

# Iterate
results$foreach(function(r) {
  cat(sprintf("%s: %.3f\n", r$id, r$score))
})
```

## Advanced Usage

### VectrixDB Class

``` r
# Create database
vdb <- VectrixDB$new("./my_data")

# Create collection with custom dimension
collection <- vdb$create_collection("docs", dimension = 100)

# Add vectors directly
collection$add(
  ids = c("doc1", "doc2"),
  vectors = matrix(rnorm(200), nrow = 2),
  metadata = list(list(a = 1), list(a = 2)),
  texts = c("First doc", "Second doc")
)

# Search
results <- collection$search(query_vector, limit = 10)

# Hybrid search
results <- collection$hybrid_search(
  query = query_vector,
  query_text = "search terms",
  limit = 10
)
```

### REST API Server

``` r
# Start server (requires plumber package)
vectrix_serve(path = "./my_data", port = 7377)
```

## Performance Tips

1.  **Use RcppAnnoy for large collections** - Automatically enabled for
    100+ docs
2.  **Pre-train embeddings** - Fit the embedder once on your corpus
3.  **Use word vectors** - GloVe/word2vec provides better semantic
    search than TF-IDF

``` r
# Pre-train for better embeddings
db <- Vectrix$new("docs")
# Add your corpus - TF-IDF will fit automatically
db$add(large_corpus)
# Now searches will use the fitted vocabulary
```

## Comparison with Python Version

| Feature            | R Version       | Python Version  |
|--------------------|-----------------|-----------------|
| Dependencies       | Pure R          | Python + ONNX   |
| Default embeddings | TF-IDF          | MiniLM (neural) |
| Word vectors       | GloVe, word2vec | Same + more     |
| ANN indexing       | RcppAnnoy       | usearch         |
| API server         | plumber         | FastAPI         |

The R version prioritizes simplicity and R-native tools. For neural
embeddings, use the Python version or provide custom embed_fn.

## Dependencies

**Required:** - R6 - OOP classes - text2vec - TF-IDF, BM25, vocabulary -
digest - ID generation - Matrix - Sparse matrices

**Optional:** - RcppAnnoy - Fast ANN search - word2vec - Load word2vec
models - stopwords - Remove stopwords - RSQLite, DBI - Persistent
storage - plumber - REST API

## License

Apache License 2.0

## Author

Kwadwo Daddy Nyame Owusu Boakye

## Links

- [GitHub](https://github.com/knowusuboaky/vectrixdb-r)
- [Python Package](https://pypi.org/project/vectrixdb/)
