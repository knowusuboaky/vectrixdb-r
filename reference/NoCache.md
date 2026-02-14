# No-Op Cache

Disabled cache (no caching)

## Super class

[`VectrixDB::BaseCache`](https://knowusuboaky.github.io/vectrixdb-r/reference/BaseCache.md)
-\> `NoCache`

## Methods

### Public methods

- [`NoCache$get()`](#method-NoCache-get)

- [`NoCache$set()`](#method-NoCache-set)

- [`NoCache$delete()`](#method-NoCache-delete)

- [`NoCache$exists()`](#method-NoCache-exists)

- [`NoCache$clear()`](#method-NoCache-clear)

- [`NoCache$size()`](#method-NoCache-size)

- [`NoCache$clone()`](#method-NoCache-clone)

Inherited methods

- [`VectrixDB::BaseCache$delete_many()`](https://knowusuboaky.github.io/vectrixdb-r/reference/BaseCache.html#method-delete_many)
- [`VectrixDB::BaseCache$get_many()`](https://knowusuboaky.github.io/vectrixdb-r/reference/BaseCache.html#method-get_many)
- [`VectrixDB::BaseCache$initialize()`](https://knowusuboaky.github.io/vectrixdb-r/reference/BaseCache.html#method-initialize)
- [`VectrixDB::BaseCache$make_key()`](https://knowusuboaky.github.io/vectrixdb-r/reference/BaseCache.html#method-make_key)
- [`VectrixDB::BaseCache$set_many()`](https://knowusuboaky.github.io/vectrixdb-r/reference/BaseCache.html#method-set_many)

------------------------------------------------------------------------

### Method [`get()`](https://rdrr.io/r/base/get.html)

Get value from cache (always returns NULL)

#### Usage

    NoCache$get(key)

#### Arguments

- `key`:

  Cache key

#### Returns

NULL

------------------------------------------------------------------------

### Method `set()`

Set cache value (no-op)

#### Usage

    NoCache$set(key, value, ttl = NULL)

#### Arguments

- `key`:

  Cache key

- `value`:

  Value to cache

- `ttl`:

  Time-to-live in seconds (ignored)

#### Returns

Invisibly returns NULL

------------------------------------------------------------------------

### Method `delete()`

Delete key from cache (always FALSE)

#### Usage

    NoCache$delete(key)

#### Arguments

- `key`:

  Cache key

#### Returns

FALSE

------------------------------------------------------------------------

### Method [`exists()`](https://rdrr.io/r/base/exists.html)

Check if key exists (always FALSE)

#### Usage

    NoCache$exists(key)

#### Arguments

- `key`:

  Cache key

#### Returns

FALSE

------------------------------------------------------------------------

### Method `clear()`

Clear cache (no-op)

#### Usage

    NoCache$clear()

#### Returns

Invisibly returns NULL

------------------------------------------------------------------------

### Method `size()`

Get cache size (always 0)

#### Usage

    NoCache$size()

#### Returns

Integer zero

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    NoCache$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
