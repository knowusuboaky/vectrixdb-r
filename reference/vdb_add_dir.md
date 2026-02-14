# Batch Add from Directory

Add all text files from a directory

## Usage

``` r
vdb_add_dir(db, dir_path, pattern = "\\.txt$", recursive = TRUE)
```

## Arguments

- db:

  Vectrix object or collection name

- dir_path:

  Directory path

- pattern:

  File pattern (default: "\*.txt")

- recursive:

  Search subdirectories

## Value

Vectrix object

## Examples

``` r
if (FALSE) { # \dontrun{
vdb_add_dir(db, "./documents/")
} # }
```
