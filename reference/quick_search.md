# Quick search - Index texts and search immediately

Quick search - Index texts and search immediately

## Usage

``` r
quick_search(texts, query, limit = 5)
```

## Arguments

- texts:

  Character vector of texts to index

- query:

  Search query

- limit:

  Number of results

## Value

Results object

## Examples

``` r
if (FALSE) { # \dontrun{
results <- quick_search(
  texts = c("Python is great", "Java is verbose", "Rust is fast"),
  query = "programming language"
)
print(results$top()$text)
} # }
```
