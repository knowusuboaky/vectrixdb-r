# Add Documents

Add documents to a collection

## Usage

``` r
vdb_add(db, texts, metadata = NULL, ids = NULL)
```

## Arguments

- db:

  Vectrix object or collection name

- texts:

  Character vector of texts

- metadata:

  Optional metadata

- ids:

  Optional IDs

## Value

Vectrix object

## Examples

``` r
if (FALSE) { # \dontrun{
vdb_add(db, c("Document 1", "Document 2"))
vdb_add("my_docs", c("Another doc"))
} # }
```
