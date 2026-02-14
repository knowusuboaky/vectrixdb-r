# Facet Result

Result of facet aggregation

## Public fields

- `field`:

  Field name

- `values`:

  List of FacetValue objects

- `total_count`:

  Total count

- `other_count`:

  Count of values not in top-N

## Methods

### Public methods

- [`FacetResult$new()`](#method-FacetResult-new)

- [`FacetResult$to_list()`](#method-FacetResult-to_list)

- [`FacetResult$clone()`](#method-FacetResult-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new FacetResult

#### Usage

    FacetResult$new(field, values, total_count, other_count = 0)

#### Arguments

- `field`:

  Field name

- `values`:

  List of FacetValue objects

- `total_count`:

  Total count

- `other_count`:

  Other count

------------------------------------------------------------------------

### Method `to_list()`

Convert to list

#### Usage

    FacetResult$to_list()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FacetResult$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
