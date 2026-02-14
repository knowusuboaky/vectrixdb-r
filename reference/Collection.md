# Collection Class

Vector collection with indexing and search

## Public fields

- `name`:

  Collection name

- `dimension`:

  Vector dimension

- `metric`:

  Distance metric

- `language`:

  Language setting ("en" or "ml")

## Methods

### Public methods

- [`Collection$new()`](#method-Collection-new)

- [`Collection$add()`](#method-Collection-add)

- [`Collection$search()`](#method-Collection-search)

- [`Collection$keyword_search()`](#method-Collection-keyword_search)

- [`Collection$hybrid_search()`](#method-Collection-hybrid_search)

- [`Collection$get()`](#method-Collection-get)

- [`Collection$delete()`](#method-Collection-delete)

- [`Collection$count()`](#method-Collection-count)

- [`Collection$clear()`](#method-Collection-clear)

- [`Collection$clone()`](#method-Collection-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new Collection

#### Usage

    Collection$new(
      name,
      dimension,
      metric = "cosine",
      storage = NULL,
      language = "en"
    )

#### Arguments

- `name`:

  Collection name

- `dimension`:

  Vector dimension

- `metric`:

  Distance metric

- `storage`:

  Storage backend

- `language`:

  Language behavior ("en" = ASCII-focused, "ml" = Unicode-aware)

------------------------------------------------------------------------

### Method `add()`

Add documents to collection

#### Usage

    Collection$add(ids, vectors, metadata = NULL, texts = NULL)

#### Arguments

- `ids`:

  Document IDs

- `vectors`:

  Matrix of vectors

- `metadata`:

  List of metadata

- `texts`:

  Character vector of texts

------------------------------------------------------------------------

### Method [`search()`](https://rdrr.io/r/base/search.html)

Search collection

#### Usage

    Collection$search(query, limit = 10, filter = NULL, include_vectors = FALSE)

#### Arguments

- `query`:

  Query vector

- `limit`:

  Number of results

- `filter`:

  Metadata filter

- `include_vectors`:

  Include vectors in results

#### Returns

Results object

------------------------------------------------------------------------

### Method `keyword_search()`

Keyword search

#### Usage

    Collection$keyword_search(query_text, limit = 10, filter = NULL)

#### Arguments

- `query_text`:

  Query text

- `limit`:

  Number of results

- `filter`:

  Metadata filter

#### Returns

Results object

------------------------------------------------------------------------

### Method `hybrid_search()`

Hybrid search (dense + sparse)

#### Usage

    Collection$hybrid_search(
      query,
      query_text,
      limit = 10,
      vector_weight = 0.5,
      text_weight = 0.5,
      filter = NULL,
      include_vectors = FALSE,
      rrf_k = 60,
      prefetch_multiplier = 10
    )

#### Arguments

- `query`:

  Query vector

- `query_text`:

  Query text

- `limit`:

  Number of results

- `vector_weight`:

  Weight for vector search

- `text_weight`:

  Weight for text search

- `filter`:

  Metadata filter

- `include_vectors`:

  Include vectors in results

- `rrf_k`:

  RRF constant

- `prefetch_multiplier`:

  Prefetch multiplier

#### Returns

Results object

------------------------------------------------------------------------

### Method [`get()`](https://rdrr.io/r/base/get.html)

Get documents by ID

#### Usage

    Collection$get(ids)

#### Arguments

- `ids`:

  Document IDs

#### Returns

List of results

------------------------------------------------------------------------

### Method `delete()`

Delete documents by ID

#### Usage

    Collection$delete(ids)

#### Arguments

- `ids`:

  Document IDs to delete

------------------------------------------------------------------------

### Method `count()`

Get document count

#### Usage

    Collection$count()

#### Returns

Integer count

------------------------------------------------------------------------

### Method `clear()`

Clear collection

#### Usage

    Collection$clear()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Collection$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
