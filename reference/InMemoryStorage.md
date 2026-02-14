# In-Memory Storage

Fast in-memory storage backend

## Public fields

- `data`:

  Storage data

## Methods

### Public methods

- [`InMemoryStorage$new()`](#method-InMemoryStorage-new)

- [`InMemoryStorage$set()`](#method-InMemoryStorage-set)

- [`InMemoryStorage$get()`](#method-InMemoryStorage-get)

- [`InMemoryStorage$delete()`](#method-InMemoryStorage-delete)

- [`InMemoryStorage$exists()`](#method-InMemoryStorage-exists)

- [`InMemoryStorage$keys()`](#method-InMemoryStorage-keys)

- [`InMemoryStorage$clear()`](#method-InMemoryStorage-clear)

- [`InMemoryStorage$count()`](#method-InMemoryStorage-count)

- [`InMemoryStorage$clone()`](#method-InMemoryStorage-clone)

------------------------------------------------------------------------

### Method `new()`

Create new in-memory storage

#### Usage

    InMemoryStorage$new()

------------------------------------------------------------------------

### Method `set()`

Store a value

#### Usage

    InMemoryStorage$set(key, value)

#### Arguments

- `key`:

  Storage key

- `value`:

  Value to store

------------------------------------------------------------------------

### Method [`get()`](https://rdrr.io/r/base/get.html)

Retrieve a value

#### Usage

    InMemoryStorage$get(key)

#### Arguments

- `key`:

  Storage key

#### Returns

Stored value or NULL

------------------------------------------------------------------------

### Method `delete()`

Delete a value

#### Usage

    InMemoryStorage$delete(key)

#### Arguments

- `key`:

  Storage key

------------------------------------------------------------------------

### Method [`exists()`](https://rdrr.io/r/base/exists.html)

Check if key exists

#### Usage

    InMemoryStorage$exists(key)

#### Arguments

- `key`:

  Storage key

#### Returns

Logical

------------------------------------------------------------------------

### Method `keys()`

List all keys

#### Usage

    InMemoryStorage$keys()

#### Returns

Character vector of keys

------------------------------------------------------------------------

### Method `clear()`

Clear all data

#### Usage

    InMemoryStorage$clear()

------------------------------------------------------------------------

### Method `count()`

Get count of stored items

#### Usage

    InMemoryStorage$count()

#### Returns

Integer count

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    InMemoryStorage$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
