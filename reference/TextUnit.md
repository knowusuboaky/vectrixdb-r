# Text Unit

A chunk of text from a document

## Public fields

- `id`:

  Unique identifier

- `text`:

  Text content

- `document_id`:

  Source document

- `chunk_index`:

  Index in document

- `start_char`:

  Start position

- `end_char`:

  End position

- `metadata`:

  Additional metadata

## Methods

### Public methods

- [`TextUnit$new()`](#method-TextUnit-new)

- [`TextUnit$clone()`](#method-TextUnit-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new TextUnit

#### Usage

    TextUnit$new(
      id,
      text,
      document_id = NULL,
      chunk_index = 0,
      start_char = 0,
      end_char = 0,
      metadata = NULL
    )

#### Arguments

- `id`:

  Unique ID

- `text`:

  Content

- `document_id`:

  Source doc

- `chunk_index`:

  Index

- `start_char`:

  Start

- `end_char`:

  End

- `metadata`:

  Metadata

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TextUnit$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
