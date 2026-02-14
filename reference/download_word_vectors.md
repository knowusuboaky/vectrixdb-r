# Download pre-trained word vectors

Downloads GloVe or fastText word vectors

## Usage

``` r
download_word_vectors(model = "glove-100", dest_dir = NULL, overwrite = FALSE)
```

## Arguments

- model:

  Model to download: "glove-50", "glove-100", "glove-200", "glove-300",
  "glove-twitter-25", "glove-twitter-50", "glove-twitter-100",
  "glove-twitter-200"

- dest_dir:

  Destination directory (default: user cache)

- overwrite:

  Overwrite existing files

## Value

Path to the downloaded vectors file

## Examples

``` r
if (FALSE) { # \dontrun{
# Download 100-dimensional GloVe vectors (~130MB)
path <- download_word_vectors("glove-100")

# Use with Vectrix
db <- Vectrix$new("docs", model = "glove", model_path = path)
} # }
```
