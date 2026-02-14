# Facet Aggregator

Faceted search aggregator for computing aggregations/counts

## Methods

### Public methods

- [`FacetAggregator$aggregate()`](#method-FacetAggregator-aggregate)

- [`FacetAggregator$to_list()`](#method-FacetAggregator-to_list)

- [`FacetAggregator$clone()`](#method-FacetAggregator-clone)

------------------------------------------------------------------------

### Method [`aggregate()`](https://rdrr.io/r/stats/aggregate.html)

Aggregate facet values from documents

#### Usage

    FacetAggregator$aggregate(documents, facet_configs)

#### Arguments

- `documents`:

  List of documents with metadata

- `facet_configs`:

  List of field names or FacetConfig objects

#### Returns

Named list mapping field names to FacetResult

------------------------------------------------------------------------

### Method `to_list()`

Convert facet results to list format

#### Usage

    FacetAggregator$to_list(facet_results)

#### Arguments

- `facet_results`:

  Named list of FacetResult objects

#### Returns

List format suitable for JSON

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FacetAggregator$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
aggregator <- FacetAggregator$new()
facets <- aggregator$aggregate(
  documents = list(
    list(category = "tech", author = "Alice"),
    list(category = "science", author = "Bob")
  ),
  facet_fields = c("category", "author")
)
} # }
```
