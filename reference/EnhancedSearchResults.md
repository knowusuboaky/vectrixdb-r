# Enhanced Search Results

Search results with enterprise features

## Public fields

- `results`:

  List of result items

- `facets`:

  Named list of FacetResult objects

- `total_count`:

  Total results before filtering

- `filtered_count`:

  Results after ACL filtering

- `query_time_ms`:

  Query time in milliseconds

- `rerank_time_ms`:

  Rerank time in milliseconds

- `facet_time_ms`:

  Facet time in milliseconds

## Methods

### Public methods

- [`EnhancedSearchResults$new()`](#method-EnhancedSearchResults-new)

- [`EnhancedSearchResults$to_list()`](#method-EnhancedSearchResults-to_list)

- [`EnhancedSearchResults$clone()`](#method-EnhancedSearchResults-clone)

------------------------------------------------------------------------

### Method `new()`

Create new EnhancedSearchResults

#### Usage

    EnhancedSearchResults$new(
      results,
      facets = list(),
      total_count = 0,
      filtered_count = 0,
      query_time_ms = 0,
      rerank_time_ms = 0,
      facet_time_ms = 0
    )

#### Arguments

- `results`:

  List of results

- `facets`:

  Named list of FacetResult

- `total_count`:

  Total count

- `filtered_count`:

  Filtered count

- `query_time_ms`:

  Query time

- `rerank_time_ms`:

  Rerank time

- `facet_time_ms`:

  Facet time

------------------------------------------------------------------------

### Method `to_list()`

Convert to list

#### Usage

    EnhancedSearchResults$to_list()

#### Returns

List representation

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    EnhancedSearchResults$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
