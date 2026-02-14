# File Cache

File-based persistent cache using RDS files

## Super class

[`VectrixDB::BaseCache`](https://knowusuboaky.github.io/vectrixdb-r/reference/BaseCache.md)
-\> `FileCache`

## Methods

### Public methods

- [`FileCache$new()`](#method-FileCache-new)

- [`FileCache$get()`](#method-FileCache-get)

- [`FileCache$set()`](#method-FileCache-set)

- [`FileCache$delete()`](#method-FileCache-delete)

- [`FileCache$exists()`](#method-FileCache-exists)

- [`FileCache$clear()`](#method-FileCache-clear)

- [`FileCache$size()`](#method-FileCache-size)

- [`FileCache$cleanup_expired()`](#method-FileCache-cleanup_expired)

- [`FileCache$clone()`](#method-FileCache-clone)

Inherited methods

- [`VectrixDB::BaseCache$delete_many()`](https://knowusuboaky.github.io/vectrixdb-r/reference/BaseCache.html#method-delete_many)
- [`VectrixDB::BaseCache$get_many()`](https://knowusuboaky.github.io/vectrixdb-r/reference/BaseCache.html#method-get_many)
- [`VectrixDB::BaseCache$make_key()`](https://knowusuboaky.github.io/vectrixdb-r/reference/BaseCache.html#method-make_key)
- [`VectrixDB::BaseCache$set_many()`](https://knowusuboaky.github.io/vectrixdb-r/reference/BaseCache.html#method-set_many)

------------------------------------------------------------------------

### Method `new()`

Create a new FileCache

#### Usage

    FileCache$new(config = NULL)

#### Arguments

- `config`:

  CacheConfig object

------------------------------------------------------------------------

### Method [`get()`](https://rdrr.io/r/base/get.html)

Get value from cache

#### Usage

    FileCache$get(key)

#### Arguments

- `key`:

  Cache key

#### Returns

Value or NULL

------------------------------------------------------------------------

### Method `set()`

Set value in cache

#### Usage

    FileCache$set(key, value, ttl = NULL)

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

    FileCache$delete(key)

#### Arguments

- `key`:

  Cache key

#### Returns

Logical success

------------------------------------------------------------------------

### Method [`exists()`](https://rdrr.io/r/base/exists.html)

Check if key exists

#### Usage

    FileCache$exists(key)

#### Arguments

- `key`:

  Cache key

#### Returns

Logical

------------------------------------------------------------------------

### Method `clear()`

Clear cache

#### Usage

    FileCache$clear()

------------------------------------------------------------------------

### Method `size()`

Get cache size

#### Usage

    FileCache$size()

#### Returns

Integer

------------------------------------------------------------------------

### Method `cleanup_expired()`

Cleanup expired entries

#### Usage

    FileCache$cleanup_expired()

#### Returns

Integer count removed

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FileCache$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
