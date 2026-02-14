# HNSW Index

High-performance approximate nearest neighbor index

## Public fields

- `dimension`:

  Vector dimension

- `metric`:

  Distance metric

- `n_trees`:

  Number of trees (for Annoy)

- `search_k`:

  Search parameter

## Methods

### Public methods

- [`HNSWIndex$new()`](#method-HNSWIndex-new)

- [`HNSWIndex$add_items()`](#method-HNSWIndex-add_items)

- [`HNSWIndex$build()`](#method-HNSWIndex-build)

- [`HNSWIndex$search()`](#method-HNSWIndex-search)

- [`HNSWIndex$get_vector()`](#method-HNSWIndex-get_vector)

- [`HNSWIndex$get_ids()`](#method-HNSWIndex-get_ids)

- [`HNSWIndex$size()`](#method-HNSWIndex-size)

- [`HNSWIndex$remove_items()`](#method-HNSWIndex-remove_items)

- [`HNSWIndex$clear()`](#method-HNSWIndex-clear)

- [`HNSWIndex$save()`](#method-HNSWIndex-save)

- [`HNSWIndex$load()`](#method-HNSWIndex-load)

- [`HNSWIndex$clone()`](#method-HNSWIndex-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new HNSWIndex

#### Usage

    HNSWIndex$new(dimension, metric = "angular", n_trees = 50, search_k = -1)

#### Arguments

- `dimension`:

  Vector dimension

- `metric`:

  Distance metric: "angular", "euclidean", "manhattan", "dot"

- `n_trees`:

  Number of trees for index (higher = more accuracy)

- `search_k`:

  Search parameter (higher = more accuracy, -1 = auto)

------------------------------------------------------------------------

### Method `add_items()`

Add items to the index

#### Usage

    HNSWIndex$add_items(ids, vectors)

#### Arguments

- `ids`:

  Character vector of IDs

- `vectors`:

  Matrix of vectors (rows = items)

#### Returns

Self

------------------------------------------------------------------------

### Method `build()`

Build the index (required before searching)

#### Usage

    HNSWIndex$build()

#### Returns

Self

------------------------------------------------------------------------

### Method [`search()`](https://rdrr.io/r/base/search.html)

Search for nearest neighbors

#### Usage

    HNSWIndex$search(query, k = 10, include_distances = TRUE)

#### Arguments

- `query`:

  Query vector

- `k`:

  Number of neighbors

- `include_distances`:

  Return distances

#### Returns

Data frame with id, distance columns

------------------------------------------------------------------------

### Method `get_vector()`

Get vector by ID

#### Usage

    HNSWIndex$get_vector(id)

#### Arguments

- `id`:

  Item ID

#### Returns

Vector or NULL

------------------------------------------------------------------------

### Method `get_ids()`

Get all IDs

#### Usage

    HNSWIndex$get_ids()

#### Returns

Character vector

------------------------------------------------------------------------

### Method `size()`

Get item count

#### Usage

    HNSWIndex$size()

#### Returns

Integer

------------------------------------------------------------------------

### Method `remove_items()`

Remove items from index

#### Usage

    HNSWIndex$remove_items(ids)

#### Arguments

- `ids`:

  IDs to remove

#### Returns

Self

------------------------------------------------------------------------

### Method `clear()`

Clear the index

#### Usage

    HNSWIndex$clear()

#### Returns

Self

------------------------------------------------------------------------

### Method [`save()`](https://rdrr.io/r/base/save.html)

Save index to file

#### Usage

    HNSWIndex$save(path)

#### Arguments

- `path`:

  File path

------------------------------------------------------------------------

### Method [`load()`](https://rdrr.io/r/base/load.html)

Load index from file

#### Usage

    HNSWIndex$load(path)

#### Arguments

- `path`:

  File path

#### Returns

Self

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    HNSWIndex$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create index
index <- HNSWIndex$new(dimension = 128, metric = "angular")

# Add vectors
index$add_items(ids = c("a", "b", "c"),
                vectors = matrix(rnorm(384), nrow = 3))

# Search
results <- index$search(query = rnorm(128), k = 5)
} # }
```
