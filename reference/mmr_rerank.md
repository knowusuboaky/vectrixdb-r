# Maximal Marginal Relevance (MMR) reranking

Maximal Marginal Relevance (MMR) reranking

## Usage

``` r
mmr_rerank(
  query_vector,
  doc_vectors,
  doc_ids,
  scores,
  lambda = 0.7,
  limit = 10
)
```

## Arguments

- query_vector:

  Query embedding

- doc_vectors:

  Matrix of document embeddings

- doc_ids:

  Vector of document IDs

- scores:

  Initial relevance scores

- lambda:

  Diversity parameter (0-1)

- limit:

  Number of results to return

## Value

Data frame with reranked results
