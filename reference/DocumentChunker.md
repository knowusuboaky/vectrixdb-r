# Document Chunker

Splits documents into text units

## Public fields

- `chunk_size`:

  Target chunk size

- `chunk_overlap`:

  Overlap size

- `by_sentence`:

  Preserve sentences

## Methods

### Public methods

- [`DocumentChunker$new()`](#method-DocumentChunker-new)

- [`DocumentChunker$chunk()`](#method-DocumentChunker-chunk)

- [`DocumentChunker$clone()`](#method-DocumentChunker-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new DocumentChunker

#### Usage

    DocumentChunker$new(chunk_size = 1200, chunk_overlap = 100, by_sentence = TRUE)

#### Arguments

- `chunk_size`:

  Target size

- `chunk_overlap`:

  Overlap

- `by_sentence`:

  Preserve sentences

------------------------------------------------------------------------

### Method `chunk()`

Chunk a document

#### Usage

    DocumentChunker$chunk(text, document_id = NULL)

#### Arguments

- `text`:

  Document text

- `document_id`:

  Document ID

#### Returns

List of TextUnit objects

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DocumentChunker$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
