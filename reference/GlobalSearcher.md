# Global Searcher

Community-based graph search

## Public fields

- `communities`:

  List of communities

- `k`:

  Number of communities

## Methods

### Public methods

- [`GlobalSearcher$new()`](#method-GlobalSearcher-new)

- [`GlobalSearcher$search()`](#method-GlobalSearcher-search)

- [`GlobalSearcher$clone()`](#method-GlobalSearcher-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new GlobalSearcher

#### Usage

    GlobalSearcher$new(communities, k = 5)

#### Arguments

- `communities`:

  List of Community objects

- `k`:

  Number of communities

------------------------------------------------------------------------

### Method [`search()`](https://rdrr.io/r/base/search.html)

Search communities

#### Usage

    GlobalSearcher$search(query)

#### Arguments

- `query`:

  Query string

#### Returns

GlobalSearchResult

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    GlobalSearcher$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
