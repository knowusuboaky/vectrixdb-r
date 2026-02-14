# Search Results Collection

Collection of search results with convenient accessors

## Public fields

- `items`:

  List of Result objects

- `query`:

  Search query

- `mode`:

  Search mode

- `time_ms`:

  Execution time in ms

## Methods

### Public methods

- [`Results$new()`](#method-Results-new)

- [`Results$length()`](#method-Results-length)

- [`Results$texts()`](#method-Results-texts)

- [`Results$ids()`](#method-Results-ids)

- [`Results$scores()`](#method-Results-scores)

- [`Results$top()`](#method-Results-top)

- [`Results$get()`](#method-Results-get)

- [`Results$foreach()`](#method-Results-foreach)

- [`Results$print()`](#method-Results-print)

- [`Results$clone()`](#method-Results-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new Results object

#### Usage

    Results$new(items = list(), query = "", mode = "hybrid", time_ms = 0)

#### Arguments

- `items`:

  List of Result objects

- `query`:

  Search query string

- `mode`:

  Search mode used

- `time_ms`:

  Execution time in milliseconds

------------------------------------------------------------------------

### Method [`length()`](https://rdrr.io/r/base/length.html)

Get number of results

#### Usage

    Results$length()

------------------------------------------------------------------------

### Method `texts()`

Get all result texts

#### Usage

    Results$texts()

#### Returns

Character vector of texts

------------------------------------------------------------------------

### Method `ids()`

Get all result IDs

#### Usage

    Results$ids()

#### Returns

Character vector of IDs

------------------------------------------------------------------------

### Method `scores()`

Get all scores

#### Usage

    Results$scores()

#### Returns

Numeric vector of scores

------------------------------------------------------------------------

### Method `top()`

Get top result

#### Usage

    Results$top()

#### Returns

Result object or NULL if empty

------------------------------------------------------------------------

### Method [`get()`](https://rdrr.io/r/base/get.html)

Get result by index

#### Usage

    Results$get(i)

#### Arguments

- `i`:

  Index

#### Returns

Result object

------------------------------------------------------------------------

### Method `foreach()`

Iterate over results

#### Usage

    Results$foreach(fn)

#### Arguments

- `fn`:

  Function to apply to each result

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print results summary

#### Usage

    Results$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Results$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
