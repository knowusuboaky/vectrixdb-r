# Simple Stemmer

Simple suffix-stripping stemmer (no external dependencies)

## Public fields

- `suffixes`:

  List of suffixes to remove

## Methods

### Public methods

- [`SimpleStemmer$stem()`](#method-SimpleStemmer-stem)

- [`SimpleStemmer$stem_words()`](#method-SimpleStemmer-stem_words)

- [`SimpleStemmer$clone()`](#method-SimpleStemmer-clone)

------------------------------------------------------------------------

### Method [`stem()`](https://rdrr.io/r/graphics/stem.html)

Stem a word

#### Usage

    SimpleStemmer$stem(word)

#### Arguments

- `word`:

  Word to stem

#### Returns

Stemmed word

------------------------------------------------------------------------

### Method `stem_words()`

Stem multiple words

#### Usage

    SimpleStemmer$stem_words(words)

#### Arguments

- `words`:

  Character vector

#### Returns

Stemmed words

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SimpleStemmer$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
