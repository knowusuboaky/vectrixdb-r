# Global Search Result

Result from global community search

## Public fields

- `communities`:

  Matching communities

- `summaries`:

  Community summaries

- `context`:

  Combined context

- `score`:

  Relevance score

## Methods

### Public methods

- [`GlobalSearchResult$new()`](#method-GlobalSearchResult-new)

- [`GlobalSearchResult$clone()`](#method-GlobalSearchResult-clone)

------------------------------------------------------------------------

### Method `new()`

Create new GlobalSearchResult

#### Usage

    GlobalSearchResult$new(
      communities = list(),
      summaries = character(0),
      context = NULL,
      score = 0
    )

#### Arguments

- `communities`:

  Communities

- `summaries`:

  Summaries

- `context`:

  Context

- `score`:

  Score

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    GlobalSearchResult$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
