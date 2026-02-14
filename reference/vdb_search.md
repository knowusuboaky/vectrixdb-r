# Search Collection

Search a collection

## Usage

``` r
vdb_search(db, query, limit = 10, mode = "hybrid", show = TRUE)
```

## Arguments

- db:

  Vectrix object or collection name

- query:

  Search query

- limit:

  Number of results

- mode:

  Search mode: "dense", "sparse", "hybrid", "ultimate"

- show:

  Print results

## Value

Results object

## Examples

``` r
if (FALSE) { # \dontrun{
results <- vdb_search(db, "machine learning")
results <- vdb_search("my_docs", "AI", limit = 5)
} # }
```
