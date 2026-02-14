# Community

A community of entities

## Public fields

- `id`:

  Community ID

- `level`:

  Hierarchy level

- `entity_ids`:

  Member entity IDs

- `summary`:

  Community summary

- `parent_id`:

  Parent community ID

- `child_ids`:

  Child community IDs

## Methods

### Public methods

- [`Community$new()`](#method-Community-new)

- [`Community$size()`](#method-Community-size)

- [`Community$clone()`](#method-Community-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new Community

#### Usage

    Community$new(
      id,
      level = 0,
      entity_ids = character(0),
      summary = NULL,
      parent_id = NULL
    )

#### Arguments

- `id`:

  ID

- `level`:

  Level

- `entity_ids`:

  Members

- `summary`:

  Summary

- `parent_id`:

  Parent

------------------------------------------------------------------------

### Method `size()`

Get size

#### Usage

    Community$size()

#### Returns

Integer

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Community$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
