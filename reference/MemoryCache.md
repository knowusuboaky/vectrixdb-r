# Memory Cache

In-memory LRU cache with TTL support

Ultra-low latency, limited by available RAM. Best for hot data, session
data, frequently accessed vectors.

## Super class

[`VectrixDB::BaseCache`](https://knowusuboaky.github.io/vectrixdb-r/reference/BaseCache.md)
-\> `MemoryCache`

## Methods

### Public methods

- [`MemoryCache$new()`](#method-MemoryCache-new)

- [`MemoryCache$get()`](#method-MemoryCache-get)

- [`MemoryCache$set()`](#method-MemoryCache-set)

- [`MemoryCache$delete()`](#method-MemoryCache-delete)

- [`MemoryCache$exists()`](#method-MemoryCache-exists)

- [`MemoryCache$clear()`](#method-MemoryCache-clear)

- [`MemoryCache$size()`](#method-MemoryCache-size)

- [`MemoryCache$cleanup_expired()`](#method-MemoryCache-cleanup_expired)

- [`MemoryCache$clone()`](#method-MemoryCache-clone)

Inherited methods

- [`VectrixDB::BaseCache$delete_many()`](https://knowusuboaky.github.io/vectrixdb-r/reference/BaseCache.html#method-delete_many)
- [`VectrixDB::BaseCache$get_many()`](https://knowusuboaky.github.io/vectrixdb-r/reference/BaseCache.html#method-get_many)
- [`VectrixDB::BaseCache$make_key()`](https://knowusuboaky.github.io/vectrixdb-r/reference/BaseCache.html#method-make_key)
- [`VectrixDB::BaseCache$set_many()`](https://knowusuboaky.github.io/vectrixdb-r/reference/BaseCache.html#method-set_many)

------------------------------------------------------------------------

### Method `new()`

Create a new MemoryCache

#### Usage

    MemoryCache$new(config = NULL)

#### Arguments

- `config`:

  CacheConfig object

------------------------------------------------------------------------

### Method [`get()`](https://rdrr.io/r/base/get.html)

Get value from cache

#### Usage

    MemoryCache$get(key)

#### Arguments

- `key`:

  Cache key

#### Returns

Value or NULL

------------------------------------------------------------------------

### Method `set()`

Set value in cache

#### Usage

    MemoryCache$set(key, value, ttl = NULL)

#### Arguments

- `key`:

  Cache key

- `value`:

  Value to cache

- `ttl`:

  Time to live

------------------------------------------------------------------------

### Method `delete()`

Delete key from cache

#### Usage

    MemoryCache$delete(key)

#### Arguments

- `key`:

  Cache key

#### Returns

Logical success

------------------------------------------------------------------------

### Method [`exists()`](https://rdrr.io/r/base/exists.html)

Check if key exists

#### Usage

    MemoryCache$exists(key)

#### Arguments

- `key`:

  Cache key

#### Returns

Logical

------------------------------------------------------------------------

### Method `clear()`

Clear cache

#### Usage

    MemoryCache$clear()

------------------------------------------------------------------------

### Method `size()`

Get cache size

#### Usage

    MemoryCache$size()

#### Returns

Integer

------------------------------------------------------------------------

### Method `cleanup_expired()`

Cleanup expired entries

#### Usage

    MemoryCache$cleanup_expired()

#### Returns

Integer count removed

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MemoryCache$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
