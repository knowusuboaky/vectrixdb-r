# Base Cache

Abstract base class for cache backends

## Public fields

- `config`:

  Cache configuration

- `stats`:

  Cache statistics

## Methods

### Public methods

- [`BaseCache$new()`](#method-BaseCache-new)

- [`BaseCache$get()`](#method-BaseCache-get)

- [`BaseCache$set()`](#method-BaseCache-set)

- [`BaseCache$delete()`](#method-BaseCache-delete)

- [`BaseCache$exists()`](#method-BaseCache-exists)

- [`BaseCache$clear()`](#method-BaseCache-clear)

- [`BaseCache$size()`](#method-BaseCache-size)

- [`BaseCache$get_many()`](#method-BaseCache-get_many)

- [`BaseCache$set_many()`](#method-BaseCache-set_many)

- [`BaseCache$delete_many()`](#method-BaseCache-delete_many)

- [`BaseCache$make_key()`](#method-BaseCache-make_key)

- [`BaseCache$clone()`](#method-BaseCache-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new cache

#### Usage

    BaseCache$new(config = NULL)

#### Arguments

- `config`:

  CacheConfig object

------------------------------------------------------------------------

### Method [`get()`](https://rdrr.io/r/base/get.html)

Get a value from cache

#### Usage

    BaseCache$get(key)

#### Arguments

- `key`:

  Cache key

#### Returns

Cached value or NULL

------------------------------------------------------------------------

### Method `set()`

Set a value in cache

#### Usage

    BaseCache$set(key, value, ttl = NULL)

#### Arguments

- `key`:

  Cache key

- `value`:

  Value to cache

- `ttl`:

  Time to live (optional)

------------------------------------------------------------------------

### Method `delete()`

Delete a key from cache

#### Usage

    BaseCache$delete(key)

#### Arguments

- `key`:

  Cache key

#### Returns

Logical success

------------------------------------------------------------------------

### Method [`exists()`](https://rdrr.io/r/base/exists.html)

Check if key exists

#### Usage

    BaseCache$exists(key)

#### Arguments

- `key`:

  Cache key

#### Returns

Logical

------------------------------------------------------------------------

### Method `clear()`

Clear all cache entries

#### Usage

    BaseCache$clear()

------------------------------------------------------------------------

### Method `size()`

Get cache size

#### Usage

    BaseCache$size()

#### Returns

Integer count

------------------------------------------------------------------------

### Method `get_many()`

Get multiple values

#### Usage

    BaseCache$get_many(keys)

#### Arguments

- `keys`:

  Character vector of keys

#### Returns

Named list of values

------------------------------------------------------------------------

### Method `set_many()`

Set multiple values

#### Usage

    BaseCache$set_many(items, ttl = NULL)

#### Arguments

- `items`:

  Named list of values

- `ttl`:

  Time to live

------------------------------------------------------------------------

### Method `delete_many()`

Delete multiple keys

#### Usage

    BaseCache$delete_many(keys)

#### Arguments

- `keys`:

  Character vector of keys

#### Returns

Integer count of deleted keys

------------------------------------------------------------------------

### Method `make_key()`

Make a prefixed key

#### Usage

    BaseCache$make_key(key)

#### Arguments

- `key`:

  Raw key

#### Returns

Prefixed key

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    BaseCache$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
