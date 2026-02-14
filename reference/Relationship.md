# Relationship

A relationship between entities

## Public fields

- `id`:

  Unique identifier

- `source_id`:

  Source entity ID

- `target_id`:

  Target entity ID

- `type`:

  Relationship type

- `description`:

  Description

- `weight`:

  Relationship weight

- `source_chunks`:

  Source chunk IDs

- `metadata`:

  Additional metadata

## Methods

### Public methods

- [`Relationship$new()`](#method-Relationship-new)

- [`Relationship$to_list()`](#method-Relationship-to_list)

- [`Relationship$clone()`](#method-Relationship-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new Relationship

#### Usage

    Relationship$new(
      source_id,
      target_id,
      type,
      description = NULL,
      weight = 1,
      source_chunks = NULL,
      metadata = NULL
    )

#### Arguments

- `source_id`:

  Source entity

- `target_id`:

  Target entity

- `type`:

  Relationship type

- `description`:

  Description

- `weight`:

  Weight

- `source_chunks`:

  Sources

- `metadata`:

  Metadata

------------------------------------------------------------------------

### Method `to_list()`

Convert to list

#### Usage

    Relationship$to_list()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Relationship$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
