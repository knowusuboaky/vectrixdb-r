# Analyzer Chain

Chain multiple analyzers together

## Public fields

- `analyzers`:

  List of TextAnalyzer objects

## Methods

### Public methods

- [`AnalyzerChain$new()`](#method-AnalyzerChain-new)

- [`AnalyzerChain$analyze()`](#method-AnalyzerChain-analyze)

- [`AnalyzerChain$clone()`](#method-AnalyzerChain-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new AnalyzerChain

#### Usage

    AnalyzerChain$new(analyzers)

#### Arguments

- `analyzers`:

  List of TextAnalyzer objects

------------------------------------------------------------------------

### Method `analyze()`

Run text through all analyzers

#### Usage

    AnalyzerChain$analyze(text)

#### Arguments

- `text`:

  Input text

#### Returns

Character vector of tokens

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    AnalyzerChain$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
