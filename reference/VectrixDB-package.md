# VectrixDB: Lightweight Visual-First Vector Database with Embedded ML Models

A lightweight, visual-first vector database with embedded ML models
requiring no API keys. Features a unique 4-tier search system (dense,
hybrid, ultimate, graph-based), GraphRAG knowledge graphs, faceted
search with ACL security, HNSW approximate nearest neighbor indexing,
caching layer, CLI tools, and built-in Shiny dashboard. Simplifies
vector database operations with zero configuration - text in, results
out.

VectrixDB is a lightweight, visual-first vector database with embedded
ML models requiring no API keys. Features a unique 4-tier search system
(dense, hybrid, ultimate, graph-based) and built-in dashboard.

## Main Functions

- [`Vectrix`](https://knowusuboaky.github.io/vectrixdb-r/reference/Vectrix.md) -
  Simple API for text-in, results-out workflow

- [`VectrixDB`](https://knowusuboaky.github.io/vectrixdb-r/reference/VectrixDB.md) -
  Advanced database interface

- [`Collection`](https://knowusuboaky.github.io/vectrixdb-r/reference/Collection.md) -
  Vector collection management

- [`vectrix_serve`](https://knowusuboaky.github.io/vectrixdb-r/reference/vectrix_serve.md) -
  Start REST API server

## Search Modes

- dense - Semantic vector search

- sparse - BM25 keyword search

- hybrid - Combined dense + sparse

- ultimate - Full pipeline with reranking

- neural - ColBERT + cross-encoder

## See also

Useful links:

- <https://github.com/knowusuboaky/vectrixdb-r>

- Report bugs at <https://github.com/knowusuboaky/vectrixdb-r/issues>

## Author

**Maintainer**: Kwadwo Daddy Nyame Owusu Boakye
<kwadwo.owusuboakye@outlook.com>

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple usage
db <- Vectrix$new("my_docs")
db$add(c("Python is great", "R is awesome", "Julia is fast"))
results <- db$search("programming language")
print(results$top()$text)
} # }
```
