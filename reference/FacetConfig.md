# Facet Configuration

Configuration for a facet field

## Public fields

- `field`:

  Field name to facet on

- `limit`:

  Max values to return

- `min_count`:

  Minimum count to include

- `sort_by`:

  Sort by "count" or "value"

- `include_zero`:

  Include zero-count values

## Methods

### Public methods

- [`FacetConfig$new()`](#method-FacetConfig-new)

- [`FacetConfig$clone()`](#method-FacetConfig-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new FacetConfig

#### Usage

    FacetConfig$new(
      field,
      limit = 10,
      min_count = 1,
      sort_by = "count",
      include_zero = FALSE
    )

#### Arguments

- `field`:

  Field name

- `limit`:

  Max values (default: 10)

- `min_count`:

  Min count (default: 1)

- `sort_by`:

  Sort method (default: "count")

- `include_zero`:

  Include zeros (default: FALSE)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FacetConfig$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
