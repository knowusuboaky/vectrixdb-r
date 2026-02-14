# Cache Statistics

Cache statistics for monitoring

## Public fields

- `hits`:

  Cache hits

- `misses`:

  Cache misses

- `sets`:

  Cache sets

- `deletes`:

  Cache deletes

- `evictions`:

  Cache evictions

## Methods

### Public methods

- [`CacheStats$record_hit()`](#method-CacheStats-record_hit)

- [`CacheStats$record_miss()`](#method-CacheStats-record_miss)

- [`CacheStats$record_set()`](#method-CacheStats-record_set)

- [`CacheStats$record_delete()`](#method-CacheStats-record_delete)

- [`CacheStats$record_eviction()`](#method-CacheStats-record_eviction)

- [`CacheStats$hit_rate()`](#method-CacheStats-hit_rate)

- [`CacheStats$to_list()`](#method-CacheStats-to_list)

- [`CacheStats$reset()`](#method-CacheStats-reset)

- [`CacheStats$clone()`](#method-CacheStats-clone)

------------------------------------------------------------------------

### Method `record_hit()`

Record a cache hit

#### Usage

    CacheStats$record_hit()

------------------------------------------------------------------------

### Method `record_miss()`

Record a cache miss

#### Usage

    CacheStats$record_miss()

------------------------------------------------------------------------

### Method `record_set()`

Record a cache set

#### Usage

    CacheStats$record_set()

------------------------------------------------------------------------

### Method `record_delete()`

Record a cache delete

#### Usage

    CacheStats$record_delete()

------------------------------------------------------------------------

### Method `record_eviction()`

Record a cache eviction

#### Usage

    CacheStats$record_eviction()

------------------------------------------------------------------------

### Method `hit_rate()`

Get hit rate

#### Usage

    CacheStats$hit_rate()

#### Returns

Numeric hit rate

------------------------------------------------------------------------

### Method `to_list()`

Convert to list

#### Usage

    CacheStats$to_list()

#### Returns

List representation

------------------------------------------------------------------------

### Method `reset()`

Reset statistics

#### Usage

    CacheStats$reset()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    CacheStats$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
