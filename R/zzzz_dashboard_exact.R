#' Resolve Dashboard Template Path
#'
#' @description Resolve the dashboard HTML template from installed package files
#' or local source checkout.
#' @return Absolute path to dashboard template HTML
#' @keywords internal
#' @noRd
vdb_dashboard_template_path <- function() {
  installed_path <- system.file("dashboard", "index.html", package = "VectrixDB")
  if (nzchar(installed_path) && file.exists(installed_path)) {
    return(installed_path)
  }

  ns_path <- tryCatch(
    getNamespaceInfo(asNamespace("VectrixDB"), "path"),
    error = function(e) ""
  )

  if (nzchar(ns_path)) {
    dev_path <- file.path(ns_path, "inst", "dashboard", "index.html")
    if (file.exists(dev_path)) {
      return(dev_path)
    }
  }

  stop(
    "Dashboard template not found. Expected 'inst/dashboard/index.html'.",
    call. = FALSE
  )
}

#' Launch VectrixDB Dashboard
#'
#' @description Start the VectrixDB API server and mount the HTML dashboard at
#' `/dashboard`.
#'
#' @param db Optional `Vectrix` object.
#' @param data_path Path to vector database directory.
#' @param port Port number (default: `7377`).
#' @param host Host address (default: `"127.0.0.1"`).
#' @param launch.browser Whether to open browser on start.
#' @param api_key Optional API key for authenticated write operations.
#'
#' @return Invisibly returns server object from `vectrix_serve()`.
#' @export
vdb_dashboard <- function(db = NULL,
                          data_path = NULL,
                          port = 7377,
                          host = "127.0.0.1",
                          launch.browser = TRUE,
                          api_key = NULL) {
  if (!requireNamespace("plumber", quietly = TRUE)) {
    stop("Package 'plumber' is required. Install with: install.packages('plumber')")
  }

  if (!is.null(db) && !is.null(db$path)) {
    data_path <- db$path
  }
  data_path <- get_data_path(data_path)

  message("============================================================")
  message("VectrixDB Dashboard")
  message("============================================================")
  message(sprintf("  Database: %s", data_path))
  message(sprintf("  Server: http://%s:%d", host, port))
  message("============================================================")
  message("\nStarting server... (Press Escape to stop)\n")

  vectrix_serve(
    path = data_path,
    host = host,
    port = port,
    api_key = api_key,
    dashboard = TRUE,
    launch.browser = launch.browser
  )
}

#' Launch Simple Dashboard
#'
#' @description Convenience wrapper around `vdb_dashboard()` using `db$path`.
#' @param db Vectrix object.
#' @return Invisibly returns server object from `vdb_dashboard()`.
#' @export
vdb_dashboard_simple <- function(db) {
  vdb_dashboard(db = db, data_path = db$path)
}
