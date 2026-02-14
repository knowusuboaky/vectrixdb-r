# Text Analyzer

Text analyzer for search indexing

Provides text processing pipelines:

- Tokenization

- Lowercasing

- Stopword removal

- Stemming

- Synonym expansion

## Public fields

- `lowercase`:

  Convert to lowercase

- `remove_stopwords`:

  Remove stopwords

- `stopwords`:

  Set of stopwords

- `stemmer`:

  Stemmer object

- `synonyms`:

  Synonym dictionary

- `min_token_length`:

  Minimum token length

- `max_token_length`:

  Maximum token length

- `token_pattern`:

  Regex pattern for tokens

## Methods

### Public methods

- [`TextAnalyzer$new()`](#method-TextAnalyzer-new)

- [`TextAnalyzer$analyze()`](#method-TextAnalyzer-analyze)

- [`TextAnalyzer$analyze_query()`](#method-TextAnalyzer-analyze_query)

- [`TextAnalyzer$clone()`](#method-TextAnalyzer-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new TextAnalyzer

#### Usage

    TextAnalyzer$new(
      lowercase = TRUE,
      remove_stopwords = FALSE,
      stopwords = NULL,
      use_stemmer = FALSE,
      synonyms = NULL,
      min_token_length = 1,
      max_token_length = 100,
      token_pattern = "[a-zA-Z0-9]+"
    )

#### Arguments

- `lowercase`:

  Lowercase text (default: TRUE)

- `remove_stopwords`:

  Remove stopwords (default: FALSE)

- `stopwords`:

  Custom stopwords (default: ENGLISH_STOPWORDS)

- `use_stemmer`:

  Use stemming (default: FALSE)

- `synonyms`:

  Named list of synonyms

- `min_token_length`:

  Min length (default: 1)

- `max_token_length`:

  Max length (default: 100)

- `token_pattern`:

  Regex pattern

------------------------------------------------------------------------

### Method `analyze()`

Analyze text and return tokens

#### Usage

    TextAnalyzer$analyze(text)

#### Arguments

- `text`:

  Input text

#### Returns

Character vector of tokens

------------------------------------------------------------------------

### Method `analyze_query()`

Analyze a query string

#### Usage

    TextAnalyzer$analyze_query(query)

#### Arguments

- `query`:

  Query text

#### Returns

Character vector of tokens

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TextAnalyzer$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
analyzer <- TextAnalyzer$english()
tokens <- analyzer$analyze("The quick brown foxes are jumping")
# c("quick", "brown", "fox", "jump")
} # }
```
