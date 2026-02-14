# Single Search Result

Represents a single search result with id, text, score, and metadata

## Public fields

- `id`:

  Document ID

- `text`:

  Document text

- `score`:

  Relevance score

- `metadata`:

  Document metadata

## Methods

### Public methods

- [`Result$new()`](#method-Result-new)

- [`Result$print()`](#method-Result-print)

- [`Result$clone()`](#method-Result-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new Result object

#### Usage

    Result$new(id, text, score, metadata = list())

#### Arguments

- `id`:

  Document ID

- `text`:

  Document text

- `score`:

  Relevance score

- `metadata`:

  Optional metadata list

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print result summary

#### Usage

    Result$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Result$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
