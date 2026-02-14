# Cache Configuration

Configuration for cache layer

## Public fields

- `backend`:

  Cache backend type

- `memory_max_size`:

  Max items in memory

- `memory_ttl_seconds`:

  Default TTL in seconds

- `file_cache_dir`:

  Directory for file cache

- `file_ttl_seconds`:

  File cache TTL

- `prefix`:

  Cache key prefix

- `compression`:

  Use compression

## Methods

### Public methods

- [`CacheConfig$new()`](#method-CacheConfig-new)

- [`CacheConfig$clone()`](#method-CacheConfig-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new CacheConfig

#### Usage

    CacheConfig$new(
      backend = "memory",
      memory_max_size = 10000,
      memory_ttl_seconds = 3600,
      file_cache_dir = NULL,
      file_ttl_seconds = 86400,
      prefix = "vectrix:",
      compression = TRUE
    )

#### Arguments

- `backend`:

  Backend type

- `memory_max_size`:

  Max memory items

- `memory_ttl_seconds`:

  Memory TTL

- `file_cache_dir`:

  File cache directory

- `file_ttl_seconds`:

  File TTL

- `prefix`:

  Key prefix

- `compression`:

  Use compression

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    CacheConfig$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
