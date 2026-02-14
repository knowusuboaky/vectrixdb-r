# Extraction Result

Result of entity extraction

## Public fields

- `entities`:

  List of Entity objects

- `relationships`:

  List of Relationship objects

- `source_chunk`:

  Source chunk ID

## Methods

### Public methods

- [`ExtractionResult$new()`](#method-ExtractionResult-new)

- [`ExtractionResult$clone()`](#method-ExtractionResult-clone)

------------------------------------------------------------------------

### Method `new()`

Create new ExtractionResult

#### Usage

    ExtractionResult$new(
      entities = list(),
      relationships = list(),
      source_chunk = NULL
    )

#### Arguments

- `entities`:

  Entities

- `relationships`:

  Relationships

- `source_chunk`:

  Source

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ExtractionResult$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
