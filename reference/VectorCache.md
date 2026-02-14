# Vector Cache

Specialized cache for vector search results

Features:

- Query result caching

- Vector embedding caching

- Automatic cache invalidation

## Public fields

- `prefix`:

  Cache key prefix

## Methods

### Public methods

- [`VectorCache$new()`](#method-VectorCache-new)

- [`VectorCache$get_search_results()`](#method-VectorCache-get_search_results)

- [`VectorCache$set_search_results()`](#method-VectorCache-set_search_results)

- [`VectorCache$get_vector()`](#method-VectorCache-get_vector)

- [`VectorCache$set_vector()`](#method-VectorCache-set_vector)

- [`VectorCache$invalidate_vector()`](#method-VectorCache-invalidate_vector)

- [`VectorCache$stats()`](#method-VectorCache-stats)

- [`VectorCache$clone()`](#method-VectorCache-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new VectorCache

#### Usage

    VectorCache$new(cache, prefix = "vec:")

#### Arguments

- `cache`:

  Base cache backend

- `prefix`:

  Key prefix (default: "vec:")

------------------------------------------------------------------------

### Method `get_search_results()`

Get cached search results

#### Usage

    VectorCache$get_search_results(collection, query, filter = NULL, limit = 10)

#### Arguments

- `collection`:

  Collection name

- `query`:

  Query vector

- `filter`:

  Filter conditions

- `limit`:

  Result limit

#### Returns

Cached results or NULL

------------------------------------------------------------------------

### Method `set_search_results()`

Cache search results

#### Usage

    VectorCache$set_search_results(
      collection,
      query,
      results,
      filter = NULL,
      limit = 10,
      ttl = 300
    )

#### Arguments

- `collection`:

  Collection name

- `query`:

  Query vector

- `results`:

  Search results

- `filter`:

  Filter conditions

- `limit`:

  Result limit

- `ttl`:

  Time to live (default: 300)

------------------------------------------------------------------------

### Method `get_vector()`

Get cached vector

#### Usage

    VectorCache$get_vector(collection, vector_id)

#### Arguments

- `collection`:

  Collection name

- `vector_id`:

  Vector ID

#### Returns

Cached vector data or NULL

------------------------------------------------------------------------

### Method `set_vector()`

Cache vector data

#### Usage

    VectorCache$set_vector(collection, vector_id, data, ttl = 3600)

#### Arguments

- `collection`:

  Collection name

- `vector_id`:

  Vector ID

- `data`:

  Vector data

- `ttl`:

  Time to live (default: 3600)

------------------------------------------------------------------------

### Method `invalidate_vector()`

Invalidate cached vector

#### Usage

    VectorCache$invalidate_vector(collection, vector_id)

#### Arguments

- `collection`:

  Collection name

- `vector_id`:

  Vector ID

------------------------------------------------------------------------

### Method `stats()`

Get cache statistics

#### Usage

    VectorCache$stats()

#### Returns

CacheStats object

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    VectorCache$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
