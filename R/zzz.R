#' @keywords internal
"_PACKAGE"

#' VectrixDB: A Lightweight Vector Database
#'
#' @description
#' VectrixDB is a lightweight vector database with embedded machine learning models
#' that do not require external API (application programming interface) keys.
#' It supports dense, hybrid, and graph-based search modes and includes a built-in
#' dashboard.
#'
#' @section Main Functions:
#' \itemize{
#'   \item \code{\link{Vectrix}} - Simple API for text-in, results-out workflow
#'   \item \code{\link{VectrixDB}} - Advanced database interface
#'   \item \code{\link{Collection}} - Vector collection management
#'   \item \code{\link{vectrix_serve}} - Start REST API server
#' }
#'
#' @section Search Modes:
#' \itemize{
#'   \item dense - Semantic vector search
#'   \item sparse - BM25 keyword search
#'   \item hybrid - Combined dense + sparse
#'   \item ultimate - Full pipeline with reranking
#'   \item neural - ColBERT + cross-encoder
#' }
#'
#' @import DBI
#' @import RSQLite
#' @import text2vec
#' @importFrom R6 R6Class
#' @importFrom Matrix sparseMatrix
#' @importFrom digest digest
#' @importFrom jsonlite fromJSON toJSON write_json
#' @importFrom stats rnorm runif setNames
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom utils download.file head object.size tail unzip
#'
#' @examples
#' \dontrun{
#' # Simple usage
#' db <- Vectrix$new("my_docs")
#' db$add(c("Python is great", "R is awesome", "Julia is fast"))
#' results <- db$search("programming language")
#' print(results$top()$text)
#' }
#'
#' @name VectrixDB-package
#' @aliases VectrixDB-package
NULL

.onLoad <- function(libname, pkgname) {
  # Set up package options
  op <- options()
  op.vectrixdb <- list(
    vectrixdb.path = default_data_path(),
    vectrixdb.default_model = "vectrixdb/all-MiniLM-L6-v2",
    vectrixdb.dimension = 384
  )
  toset <- !(names(op.vectrixdb) %in% names(op))
  if (any(toset)) options(op.vectrixdb[toset])

  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "VectrixDB v1.1.2 - The simplest vector database\n",
    "Documentation: https://github.com/knowusuboaky/vectrixdb-r"
  )
}
