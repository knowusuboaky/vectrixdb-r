# Local Searcher

Entity-based graph search

## Public fields

- `graph`:

  Knowledge graph

- `k`:

  Number of seed entities

- `traversal_depth`:

  Max hops

## Methods

### Public methods

- [`LocalSearcher$new()`](#method-LocalSearcher-new)

- [`LocalSearcher$search()`](#method-LocalSearcher-search)

- [`LocalSearcher$clone()`](#method-LocalSearcher-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new LocalSearcher

#### Usage

    LocalSearcher$new(graph, k = 10, traversal_depth = 2)

#### Arguments

- `graph`:

  KnowledgeGraph

- `k`:

  Seed entities

- `traversal_depth`:

  Max depth

------------------------------------------------------------------------

### Method [`search()`](https://rdrr.io/r/base/search.html)

Search the graph

#### Usage

    LocalSearcher$search(query)

#### Arguments

- `query`:

  Query string

#### Returns

LocalSearchResult

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LocalSearcher$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
