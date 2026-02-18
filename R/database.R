#' VectrixDB Database Class
#'
#' @description Main database interface managing collections
#'
#' @name VectrixDB
#' @aliases VectrixDB
#' @export
VectrixDB <- R6::R6Class(
  "VectrixDB",
  public = list(
    #' @field path Database storage path
    path = NULL,

    #' @description Create or open a VectrixDB database
    #' @param path Storage path
    #' @param storage_type Storage type ("memory" or "sqlite")
    initialize = function(path = NULL, storage_type = "memory") {
      self$path <- get_data_path(path)
      private$storage_type <- storage_type
      private$collections <- list()

      # Load existing collections from disk if using sqlite
      if (storage_type == "sqlite") {
        private$load_collections()
      }
    },

    #' @description Create a new collection
    #' @param name Collection name
    #' @param dimension Vector dimension
    #' @param metric Distance metric
    #' @param enable_text_index Enable text indexing
    #' @return Collection object
    create_collection = function(name, dimension, metric = "cosine",
                                  enable_text_index = TRUE) {

      if (name %in% names(private$collections)) {
        stop(sprintf("Collection '%s' already exists", name))
      }

      storage <- if (private$storage_type == "sqlite") {
        db_path <- file.path(self$path, paste0(name, ".db"))
        SQLiteStorage$new(db_path)
      } else {
        InMemoryStorage$new()
      }

      collection <- Collection$new(
        name = name,
        dimension = dimension,
        metric = metric,
        storage = storage
      )

      private$collections[[name]] <- collection
      collection
    },

    #' @description Get an existing collection
    #' @param name Collection name
    #' @return Collection object
    get_collection = function(name) {
      if (!(name %in% names(private$collections))) {
        stop(sprintf("Collection '%s' does not exist", name))
      }
      private$collections[[name]]
    },

    #' @description List all collections
    #' @return Character vector of collection names
    list_collections = function() {
      names(private$collections)
    },

    #' @description Delete a collection
    #' @param name Collection name
    delete_collection = function(name) {
      if (!(name %in% names(private$collections))) {
        stop(sprintf("Collection '%s' does not exist", name))
      }

      # Close and remove
      collection <- private$collections[[name]]
      if (private$storage_type == "sqlite") {
        # Remove database file
        db_path <- file.path(self$path, paste0(name, ".db"))
        if (file.exists(db_path)) {
          file.remove(db_path)
        }
      }

      private$collections[[name]] <- NULL
      invisible(self)
    },

    #' @description Check if collection exists
    #' @param name Collection name
    #' @return Logical
    has_collection = function(name) {
      name %in% names(private$collections)
    },

    #' @description Get database statistics
    #' @return List with stats
    stats = function() {
      list(
        path = self$path,
        storage_type = private$storage_type,
        num_collections = length(private$collections),
        collections = lapply(private$collections, function(c) {
          list(
            name = c$name,
            dimension = c$dimension,
            count = c$count()
          )
        })
      )
    },

    #' @description Close the database
    close = function() {
      for (name in names(private$collections)) {
        collection <- private$collections[[name]]
        # Close storage if applicable
      }
      private$collections <- list()
      invisible(NULL)
    },

    #' @description Print database summary
    print = function() {
      cat(sprintf("VectrixDB(path='%s', %d collections)\n",
                  self$path, length(private$collections)))
      invisible(self)
    }
  ),

  private = list(
    storage_type = NULL,
    collections = NULL,

    load_collections = function() {
      # Load existing collections from sqlite files
      db_files <- list.files(self$path, pattern = "\\.db$", full.names = TRUE)

      for (db_file in db_files) {
        name <- tools::file_path_sans_ext(basename(db_file))
        # Would need to read metadata to get dimension
        # For now, skip auto-loading
      }
    }
  )
)

#' Create or open a VectrixDB database
#'
#' @rdname VectrixDB
#' @param path Storage path
#' @param storage_type Storage type
#' @return VectrixDB object
vectrixdb <- function(path = NULL, storage_type = "memory") {
  VectrixDB$new(path, storage_type)
}
