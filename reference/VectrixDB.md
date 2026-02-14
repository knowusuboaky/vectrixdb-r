# VectrixDB Database Class

Main database interface managing collections

## Usage

``` r
vectrixdb(path = "./vectrixdb_data", storage_type = "memory")
```

## Arguments

- path:

  Storage path

- storage_type:

  Storage type

## Value

VectrixDB object

## Public fields

- `path`:

  Database storage path

## Methods

### Public methods

- [`VectrixDB$new()`](#method-VectrixDB-new)

- [`VectrixDB$create_collection()`](#method-VectrixDB-create_collection)

- [`VectrixDB$get_collection()`](#method-VectrixDB-get_collection)

- [`VectrixDB$list_collections()`](#method-VectrixDB-list_collections)

- [`VectrixDB$delete_collection()`](#method-VectrixDB-delete_collection)

- [`VectrixDB$has_collection()`](#method-VectrixDB-has_collection)

- [`VectrixDB$stats()`](#method-VectrixDB-stats)

- [`VectrixDB$close()`](#method-VectrixDB-close)

- [`VectrixDB$print()`](#method-VectrixDB-print)

- [`VectrixDB$clone()`](#method-VectrixDB-clone)

------------------------------------------------------------------------

### Method `new()`

Create or open a VectrixDB database

#### Usage

    VectrixDB$new(path = "./vectrixdb_data", storage_type = "memory")

#### Arguments

- `path`:

  Storage path

- `storage_type`:

  Storage type ("memory" or "sqlite")

------------------------------------------------------------------------

### Method `create_collection()`

Create a new collection

#### Usage

    VectrixDB$create_collection(
      name,
      dimension,
      metric = "cosine",
      enable_text_index = TRUE
    )

#### Arguments

- `name`:

  Collection name

- `dimension`:

  Vector dimension

- `metric`:

  Distance metric

- `enable_text_index`:

  Enable text indexing

#### Returns

Collection object

------------------------------------------------------------------------

### Method `get_collection()`

Get an existing collection

#### Usage

    VectrixDB$get_collection(name)

#### Arguments

- `name`:

  Collection name

#### Returns

Collection object

------------------------------------------------------------------------

### Method `list_collections()`

List all collections

#### Usage

    VectrixDB$list_collections()

#### Returns

Character vector of collection names

------------------------------------------------------------------------

### Method `delete_collection()`

Delete a collection

#### Usage

    VectrixDB$delete_collection(name)

#### Arguments

- `name`:

  Collection name

------------------------------------------------------------------------

### Method `has_collection()`

Check if collection exists

#### Usage

    VectrixDB$has_collection(name)

#### Arguments

- `name`:

  Collection name

#### Returns

Logical

------------------------------------------------------------------------

### Method `stats()`

Get database statistics

#### Usage

    VectrixDB$stats()

#### Returns

List with stats

------------------------------------------------------------------------

### Method [`close()`](https://rdrr.io/r/base/connections.html)

Close the database

#### Usage

    VectrixDB$close()

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print database summary

#### Usage

    VectrixDB$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    VectrixDB$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
