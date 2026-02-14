# Keyword Analyzer

Treats entire input as single token

## Super class

[`VectrixDB::TextAnalyzer`](https://knowusuboaky.github.io/vectrixdb-r/reference/TextAnalyzer.md)
-\> `KeywordAnalyzer`

## Methods

### Public methods

- [`KeywordAnalyzer$analyze()`](#method-KeywordAnalyzer-analyze)

- [`KeywordAnalyzer$clone()`](#method-KeywordAnalyzer-clone)

Inherited methods

- [`VectrixDB::TextAnalyzer$analyze_query()`](https://knowusuboaky.github.io/vectrixdb-r/reference/TextAnalyzer.html#method-analyze_query)
- [`VectrixDB::TextAnalyzer$initialize()`](https://knowusuboaky.github.io/vectrixdb-r/reference/TextAnalyzer.html#method-initialize)

------------------------------------------------------------------------

### Method `analyze()`

Analyze text as single keyword

#### Usage

    KeywordAnalyzer$analyze(text)

#### Arguments

- `text`:

  Input text

#### Returns

Single-element character vector

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    KeywordAnalyzer$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
