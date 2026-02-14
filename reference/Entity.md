# Entity

An extracted entity

## Public fields

- `id`:

  Unique identifier

- `name`:

  Entity name

- `type`:

  Entity type

- `description`:

  Description

- `source_chunks`:

  Source chunk IDs

- `embedding`:

  Vector embedding

- `metadata`:

  Additional metadata

## Methods

### Public methods

- [`Entity$new()`](#method-Entity-new)

- [`Entity$to_list()`](#method-Entity-to_list)

- [`Entity$clone()`](#method-Entity-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new Entity

#### Usage

    Entity$new(
      id = NULL,
      name,
      type,
      description = NULL,
      source_chunks = NULL,
      embedding = NULL,
      metadata = NULL
    )

#### Arguments

- `id`:

  Unique ID

- `name`:

  Name

- `type`:

  Type

- `description`:

  Description

- `source_chunks`:

  Sources

- `embedding`:

  Vector

- `metadata`:

  Metadata

------------------------------------------------------------------------

### Method `to_list()`

Convert to list

#### Usage

    Entity$to_list()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Entity$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
