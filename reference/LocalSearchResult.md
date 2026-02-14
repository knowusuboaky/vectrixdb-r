# Local Search Result

Result from local graph search

## Public fields

- `entities`:

  Matching entities

- `relationships`:

  Related relationships

- `subgraph`:

  Traversed subgraph

- `context`:

  Combined context text

- `score`:

  Relevance score

## Methods

### Public methods

- [`LocalSearchResult$new()`](#method-LocalSearchResult-new)

- [`LocalSearchResult$clone()`](#method-LocalSearchResult-clone)

------------------------------------------------------------------------

### Method `new()`

Create new LocalSearchResult

#### Usage

    LocalSearchResult$new(
      entities = list(),
      relationships = list(),
      subgraph = NULL,
      context = NULL,
      score = 0
    )

#### Arguments

- `entities`:

  Entities

- `relationships`:

  Relationships

- `subgraph`:

  SubGraph

- `context`:

  Context

- `score`:

  Score

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LocalSearchResult$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
