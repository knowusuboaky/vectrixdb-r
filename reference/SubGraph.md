# SubGraph

A subset of a knowledge graph

## Public fields

- `entities`:

  Entities in subgraph

- `relationships`:

  Relationships in subgraph

## Methods

### Public methods

- [`SubGraph$new()`](#method-SubGraph-new)

- [`SubGraph$to_list()`](#method-SubGraph-to_list)

- [`SubGraph$clone()`](#method-SubGraph-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new SubGraph

#### Usage

    SubGraph$new(entities = list(), relationships = list())

#### Arguments

- `entities`:

  Entities

- `relationships`:

  Relationships

------------------------------------------------------------------------

### Method `to_list()`

Convert to list

#### Usage

    SubGraph$to_list()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SubGraph$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
