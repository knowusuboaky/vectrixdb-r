# Launch VectrixDB Dashboard

Start the VectrixDB API server and mount the HTML dashboard at
`/dashboard`.

## Usage

``` r
vdb_dashboard(
  db = NULL,
  data_path = NULL,
  port = 7377,
  host = "127.0.0.1",
  launch.browser = TRUE,
  api_key = NULL
)
```

## Arguments

- db:

  Optional `Vectrix` object.

- data_path:

  Path to vector database directory.

- port:

  Port number (default: `7377`).

- host:

  Host address (default: `"127.0.0.1"`).

- launch.browser:

  Whether to open browser on start.

- api_key:

  Optional API key for authenticated write operations.

## Value

Invisibly returns server object from
[`vectrix_serve()`](https://knowusuboaky.github.io/vectrixdb-r/reference/vectrix_serve.md).
