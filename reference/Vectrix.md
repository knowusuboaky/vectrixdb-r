# VectrixDB Easy API - The Simplest Vector Database

Zero config. Text in, results out. One line for everything.

## Public fields

- `name`:

  Collection name

- `path`:

  Storage path

- `dimension`:

  Vector dimension

- `model_name`:

  Model identifier

- `model_type`:

  Model type

- `language`:

  Language setting

- `tier`:

  Storage tier

## Methods

### Public methods

- [`Vectrix$new()`](#method-Vectrix-new)

- [`Vectrix$add()`](#method-Vectrix-add)

- [`Vectrix$set_language()`](#method-Vectrix-set_language)

- [`Vectrix$search()`](#method-Vectrix-search)

- [`Vectrix$delete()`](#method-Vectrix-delete)

- [`Vectrix$clear()`](#method-Vectrix-clear)

- [`Vectrix$count()`](#method-Vectrix-count)

- [`Vectrix$get()`](#method-Vectrix-get)

- [`Vectrix$similar()`](#method-Vectrix-similar)

- [`Vectrix$close()`](#method-Vectrix-close)

- [`Vectrix$print()`](#method-Vectrix-print)

- [`Vectrix$clone()`](#method-Vectrix-clone)

------------------------------------------------------------------------

### Method `new()`

Create or open a VectrixDB collection

#### Usage

    Vectrix$new(
      name = "default",
      path = NULL,
      model = NULL,
      dimension = NULL,
      embed_fn = NULL,
      model_path = NULL,
      language = NULL,
      tier = "dense",
      auto_download = TRUE
    )

#### Arguments

- `name`:

  Collection name

- `path`:

  Storage path. Defaults to a session temp directory.

- `model`:

  Embedding model: "tfidf" (default), "glove-50", "glove-100",
  "glove-200", "glove-300", or "word2vec"

- `dimension`:

  Vector dimension (auto-detected for GloVe)

- `embed_fn`:

  Custom embedding function: fn(texts) -\> matrix

- `model_path`:

  Path to pre-trained word vectors (GloVe .txt or word2vec .bin)

- `language`:

  Language behavior: "en" (English-focused) or "ml"
  (multilingual/Unicode)

- `tier`:

  Storage tier: "dense", "hybrid", "ultimate", or "graph"

- `auto_download`:

  Automatically download GloVe vectors if needed (default: TRUE)

#### Examples

    \dontrun{
    # Default TF-IDF embeddings (no external files needed)
    db <- Vectrix$new("docs")

    # With GloVe 100d word vectors (auto-downloads ~130MB)
    db <- Vectrix$new("docs", model = "glove-100")

    # With pre-downloaded GloVe
    db <- Vectrix$new("docs", model_path = "path/to/glove.6B.100d.txt")

    # Custom embedding function
    db <- Vectrix$new("docs", embed_fn = my_embed_function, dimension = 768)
    }

------------------------------------------------------------------------

### Method `add()`

Add texts to the collection

#### Usage

    Vectrix$add(texts, metadata = NULL, ids = NULL)

#### Arguments

- `texts`:

  Single text or character vector of texts

- `metadata`:

  Optional metadata list or list of lists

- `ids`:

  Optional custom IDs

#### Returns

Self for chaining

#### Examples

    \dontrun{
    db$add(c("text 1", "text 2"))
    db$add("another text", metadata = list(source = "web"))
    }

------------------------------------------------------------------------

### Method `set_language()`

Update collection language behavior

#### Usage

    Vectrix$set_language(language = "en")

#### Arguments

- `language`:

  Language behavior: "en" or "ml"

#### Returns

Self for chaining

------------------------------------------------------------------------

### Method [`search()`](https://rdrr.io/r/base/search.html)

Search the collection

#### Usage

    Vectrix$search(
      query,
      limit = 10,
      mode = "hybrid",
      rerank = NULL,
      filter = NULL,
      diversity = 0.7
    )

#### Arguments

- `query`:

  Search query text

- `limit`:

  Number of results (default: 10)

- `mode`:

  Search mode: "dense", "sparse", "hybrid", "ultimate"

- `rerank`:

  Reranking method: NULL, "mmr", "exact", "cross-encoder"

- `filter`:

  Metadata filter

- `diversity`:

  Diversity parameter for MMR (0-1)

#### Returns

Results object with search results

#### Examples

    \dontrun{
    results <- db$search("python programming")
    results <- db$search("AI", mode = "ultimate", rerank = "mmr")
    print(results$top()$text)
    }

------------------------------------------------------------------------

### Method `delete()`

Delete documents by ID

#### Usage

    Vectrix$delete(ids)

#### Arguments

- `ids`:

  Document ID(s) to delete

#### Returns

Self for chaining

------------------------------------------------------------------------

### Method `clear()`

Clear all documents from collection

#### Usage

    Vectrix$clear()

#### Returns

Self for chaining

------------------------------------------------------------------------

### Method `count()`

Get number of documents

#### Usage

    Vectrix$count()

#### Returns

Integer count

------------------------------------------------------------------------

### Method [`get()`](https://rdrr.io/r/base/get.html)

Get documents by ID

#### Usage

    Vectrix$get(ids)

#### Arguments

- `ids`:

  Document ID(s)

#### Returns

List of Result objects

------------------------------------------------------------------------

### Method `similar()`

Find similar documents to a given document

#### Usage

    Vectrix$similar(id, limit = 10)

#### Arguments

- `id`:

  Document ID

- `limit`:

  Number of results

#### Returns

Results object

------------------------------------------------------------------------

### Method [`close()`](https://rdrr.io/r/base/connections.html)

Close the database connection

#### Usage

    Vectrix$close()

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print Vectrix summary

#### Usage

    Vectrix$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Vectrix$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create and add - ONE LINE
db <- Vectrix$new("my_docs")$add(c("Python is great", "Machine learning is fun"))

# Search - ONE LINE
results <- db$search("programming")

# Full power - STILL ONE LINE
results <- db$search("AI", mode = "ultimate")  # dense + sparse + rerank
} # }


## ------------------------------------------------
## Method `Vectrix$new`
## ------------------------------------------------

if (FALSE) { # \dontrun{
# Default TF-IDF embeddings (no external files needed)
db <- Vectrix$new("docs")

# With GloVe 100d word vectors (auto-downloads ~130MB)
db <- Vectrix$new("docs", model = "glove-100")

# With pre-downloaded GloVe
db <- Vectrix$new("docs", model_path = "path/to/glove.6B.100d.txt")

# Custom embedding function
db <- Vectrix$new("docs", embed_fn = my_embed_function, dimension = 768)
} # }

## ------------------------------------------------
## Method `Vectrix$add`
## ------------------------------------------------

if (FALSE) { # \dontrun{
db$add(c("text 1", "text 2"))
db$add("another text", metadata = list(source = "web"))
} # }

## ------------------------------------------------
## Method `Vectrix$search`
## ------------------------------------------------

if (FALSE) { # \dontrun{
results <- db$search("python programming")
results <- db$search("AI", mode = "ultimate", rerank = "mmr")
print(results$top()$text)
} # }
```
