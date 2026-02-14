# Start VectrixDB server

Launch a REST API server with optional dashboard

## Usage

``` r
vectrix_serve(
  path = "./vectrixdb_data",
  host = "127.0.0.1",
  port = 7377,
  api_key = NULL,
  dashboard = TRUE,
  launch.browser = FALSE
)
```

## Arguments

- path:

  Database path

- host:

  Host address (default: "127.0.0.1")

- port:

  Port number (default: 7377)

- api_key:

  Optional API key for authentication

- dashboard:

  Enable dashboard (default: TRUE)

- launch.browser:

  Open dashboard/docs URL in browser (default: FALSE)

## Value

Invisible NULL (server runs until stopped)

## Examples

``` r
if (FALSE) { # \dontrun{
vectrix_serve(path = "./my_data", port = 7377)
} # }
```
