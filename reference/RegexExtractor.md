# Regex Entity Extractor

Simple regex-based entity extractor (no external dependencies)

## Public fields

- `entity_types`:

  Entity types to extract

## Methods

### Public methods

- [`RegexExtractor$new()`](#method-RegexExtractor-new)

- [`RegexExtractor$extract()`](#method-RegexExtractor-extract)

- [`RegexExtractor$clone()`](#method-RegexExtractor-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new RegexExtractor

#### Usage

    RegexExtractor$new(entity_types = NULL)

#### Arguments

- `entity_types`:

  Types to extract

------------------------------------------------------------------------

### Method `extract()`

Extract entities from text

#### Usage

    RegexExtractor$extract(text, chunk_id = NULL)

#### Arguments

- `text`:

  Text to extract from

- `chunk_id`:

  Chunk ID

#### Returns

ExtractionResult

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    RegexExtractor$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
