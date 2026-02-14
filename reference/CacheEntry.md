# Cache Entry

A cached entry with metadata

## Public fields

- `value`:

  Cached value

- `created_at`:

  Creation timestamp

- `ttl`:

  Time to live in seconds

- `hits`:

  Number of hits

## Methods

### Public methods

- [`CacheEntry$new()`](#method-CacheEntry-new)

- [`CacheEntry$is_expired()`](#method-CacheEntry-is_expired)

- [`CacheEntry$clone()`](#method-CacheEntry-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new CacheEntry

#### Usage

    CacheEntry$new(value, ttl)

#### Arguments

- `value`:

  The value to cache

- `ttl`:

  Time to live in seconds

------------------------------------------------------------------------

### Method `is_expired()`

Check if entry is expired

#### Usage

    CacheEntry$is_expired()

#### Returns

Logical

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    CacheEntry$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
