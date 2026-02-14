# SQLite Storage

Persistent SQLite storage backend

## Public fields

- `db_path`:

  Database file path

- `conn`:

  Database connection

## Methods

### Public methods

- [`SQLiteStorage$new()`](#method-SQLiteStorage-new)

- [`SQLiteStorage$set()`](#method-SQLiteStorage-set)

- [`SQLiteStorage$get()`](#method-SQLiteStorage-get)

- [`SQLiteStorage$delete()`](#method-SQLiteStorage-delete)

- [`SQLiteStorage$exists()`](#method-SQLiteStorage-exists)

- [`SQLiteStorage$keys()`](#method-SQLiteStorage-keys)

- [`SQLiteStorage$clear()`](#method-SQLiteStorage-clear)

- [`SQLiteStorage$count()`](#method-SQLiteStorage-count)

- [`SQLiteStorage$close()`](#method-SQLiteStorage-close)

- [`SQLiteStorage$clone()`](#method-SQLiteStorage-clone)

------------------------------------------------------------------------

### Method `new()`

Create new SQLite storage

#### Usage

    SQLiteStorage$new(path)

#### Arguments

- `path`:

  Database file path

------------------------------------------------------------------------

### Method `set()`

Store a value

#### Usage

    SQLiteStorage$set(key, value)

#### Arguments

- `key`:

  Storage key

- `value`:

  Value to store

------------------------------------------------------------------------

### Method [`get()`](https://rdrr.io/r/base/get.html)

Retrieve a value

#### Usage

    SQLiteStorage$get(key)

#### Arguments

- `key`:

  Storage key

#### Returns

Stored value or NULL

------------------------------------------------------------------------

### Method `delete()`

Delete a value

#### Usage

    SQLiteStorage$delete(key)

#### Arguments

- `key`:

  Storage key

------------------------------------------------------------------------

### Method [`exists()`](https://rdrr.io/r/base/exists.html)

Check if key exists

#### Usage

    SQLiteStorage$exists(key)

#### Arguments

- `key`:

  Storage key

#### Returns

Logical

------------------------------------------------------------------------

### Method `keys()`

List all keys

#### Usage

    SQLiteStorage$keys()

#### Returns

Character vector of keys

------------------------------------------------------------------------

### Method `clear()`

Clear all data

#### Usage

    SQLiteStorage$clear()

------------------------------------------------------------------------

### Method `count()`

Get count of stored items

#### Usage

    SQLiteStorage$count()

#### Returns

Integer count

------------------------------------------------------------------------

### Method [`close()`](https://rdrr.io/r/base/connections.html)

Close database connection

#### Usage

    SQLiteStorage$close()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SQLiteStorage$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
