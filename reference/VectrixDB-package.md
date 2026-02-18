# VectrixDB: Lightweight Vector Database with Embedded Machine Learning Models

A lightweight vector database for text retrieval in R with embedded
machine learning models and no external API (Application Programming
Interface) keys. Supports dense and hybrid search, optional HNSW
(Hierarchical Navigable Small World) approximate nearest-neighbor
indexing, faceted filters with ACL (Access Control List) metadata,
command-line tools, and a local dashboard built with 'shiny'.

VectrixDB is a lightweight vector database with embedded machine
learning models that do not require external API (application
programming interface) keys. It supports dense, hybrid, and graph-based
search modes and includes a built-in dashboard.

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

- <https://knowusuboaky.github.io/vectrixdb-r/>

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
