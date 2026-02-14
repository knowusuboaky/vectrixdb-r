# Filter Class for Metadata Filtering

Build metadata filters for search queries

## Public fields

- `conditions`:

  List of filter conditions

## Methods

### Public methods

- [`Filter$new()`](#method-Filter-new)

- [`Filter$eq()`](#method-Filter-eq)

- [`Filter$ne()`](#method-Filter-ne)

- [`Filter$gt()`](#method-Filter-gt)

- [`Filter$lt()`](#method-Filter-lt)

- [`Filter$in_list()`](#method-Filter-in_list)

- [`Filter$to_list()`](#method-Filter-to_list)

- [`Filter$clone()`](#method-Filter-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new Filter

#### Usage

    Filter$new(...)

#### Arguments

- `...`:

  Named filter conditions

------------------------------------------------------------------------

### Method `eq()`

Add equality condition

#### Usage

    Filter$eq(field, value)

#### Arguments

- `field`:

  Field name

- `value`:

  Value to match

#### Returns

Self for chaining

------------------------------------------------------------------------

### Method `ne()`

Add not-equal condition

#### Usage

    Filter$ne(field, value)

#### Arguments

- `field`:

  Field name

- `value`:

  Value to exclude

#### Returns

Self for chaining

------------------------------------------------------------------------

### Method `gt()`

Add greater-than condition

#### Usage

    Filter$gt(field, value)

#### Arguments

- `field`:

  Field name

- `value`:

  Threshold value

#### Returns

Self for chaining

------------------------------------------------------------------------

### Method `lt()`

Add less-than condition

#### Usage

    Filter$lt(field, value)

#### Arguments

- `field`:

  Field name

- `value`:

  Threshold value

#### Returns

Self for chaining

------------------------------------------------------------------------

### Method `in_list()`

Add in-list condition

#### Usage

    Filter$in_list(field, values)

#### Arguments

- `field`:

  Field name

- `values`:

  Vector of values

#### Returns

Self for chaining

------------------------------------------------------------------------

### Method `to_list()`

Convert to list for API

#### Usage

    Filter$to_list()

#### Returns

List representation

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Filter$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
