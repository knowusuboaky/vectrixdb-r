# Create Collection

Create a new VectrixDB collection

## Usage

``` r
vdb_create(name, model = "tfidf", dimension = NULL, data_dir = NULL)
```

## Arguments

- name:

  Collection name

- model:

  Embedding model

- dimension:

  Vector dimension

- data_dir:

  Data directory

## Value

Vectrix object

## Examples

``` r
if (FALSE) { # \dontrun{
db <- vdb_create("my_docs")
} # }
```
