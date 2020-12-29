#' A Dict.table class
#'
#' @description The [Dict.table()] is a mix of a dictionary and a
#' `data.table`. In particular, it is a dictionary (inheriting all [Dict()]
#' methods) with each element having the same length.
#'
#' @author Roman Pahl
#' @import data.table
#' @seealso [Dict()], [dict.table()], [data.table::data.table()]
#' @export
Dict.table <- R6::R6Class("Dict.table",
    inherit = Dict,
    public = list(
        #' @description `Dict.table` constructor
        #' @param ... initial elements put into the `Dict.table` and further
        #' args passed to `[data.table::as.data.table()].
        #' @param asis `logical` if FALSE, the constructor of the super class
        #' is skipped, which means, checking of valid key names is skipped as
        #' well. On the other hand, this allows to initialize the `Dict.table`
        #' with a `data.table` object by reference.
        #' @return invisibly returns the `Dict.table`
        initialize = function(..., asis = FALSE) {

            if (asis) {
                if (data.table::is.data.table(...)) {
                    private$elems = list(...)[[1]]
                } else {
                    private$elems = data.table::as.data.table(...)
                }
            } else {
                super$initialize(...)
                private$elems <- do.call(data.table::data.table,
                                         args = self$values())
            }
            invisible(self)
        },

        #' @description Dimensions of the `Dict.table` object
        #' @return `integer` number of columns
        dim = function() dim(self$values()),

        #' @description delete value
        #' @param key `character` names or indices of columns to delete.
        #' @return If `key` in `Dict.table`, delete it, otherwise raise an error.
        delete = function(key) {
            missing_cols = Filter(key, f = Negate(self$has))
            if (length(missing_cols)) {
                col_str = paste0("'", missing_cols, "'", collapse = ", ")
                stop("Column", ifelse(length(missing_cols) > 1, "s ", " "),
                     col_str,
                     ifelse(is.character(key),
                            paste(" not in", data.class(self)),
                            paste0(" out of range (ncol = ", self$ncol(), ")")))
            }
            self$discard(key)
        },

        #' @description If key in `Dict.table`, delete it.
        #' @param key `character` column names or indices to discard.
        #' @return invisibly returns the `Dict.table`
        discard = function(key) {
            j = Filter(unique(key), f = self$has)
            if (is.numeric(j)) j = as.integer(j)

            if (length(j)) {
                data.table::set(self$values(), j = j, value = NULL)
            }
            invisible(self)
        },

        #' @description Determine if `Dict.table` has a `key`.
        #' @param key `character` name of key.
        #' @return `TRUE` if `key` in `Dict.table`, otherwise `FALSE`.
        has = function(key) {
            if (length(key) != 1) stop("key must be of length 1")
            if (is.na(key)) stop("undefined key")
            switch(data.class(key),
                   "character" = super$has(key),
                   "numeric" = self$ncol() >= key,
                   stop("key must be character or numeric")
            )
        },

        #' @description Number of columns
        #' @return `integer` number of columns
        ncol = function() ncol(self$values()),

        #' @description Number of rows
        #' @return `integer` number of rows
        nrow = function() nrow(self$values()),

        #' @description Peek for value in `Dict.table`.
        #' @param key `character` name of key.
        #' @param default returned default value.
        #' @return value for `key` if `key` is in the `Dict.table` else `default`.
        peek = function(key, default = NULL) {
            if (self$has(key)) {
                self$values()[[key]]
            } else {
                rep(default, times = self$nrow() %/% length(default))
            }
        },

        #' @description Print Data.table
        #' @param ... further args passed to [print.data.table()]
        print = function(...) {
            cat0 <- function(...) cat(..., sep = "")
            class_name <- paste0("<", data.class(self), ">")
            cat0(class_name, " with ",
                 self$nrow(), " row", ifelse(self$nrow() == 1, "", "s"), " and ",
                 self$ncol(), " column", ifelse(self$ncol() == 1, "", "s"), "\n")
            print(self$values(), ...)
        },

        #' @description row-bind this with another [Dict.table()] object.
        #' @param other `Dict.table` object to row-bind
        #' @param ... further args passed to [data.table::rbindlist()]
        #' @return a copy of original `Dict.table` with a copy of `other`
        #' attached at the bottom.
        rbind = function(other, ...) {
            stopifnot(inherits(other, "Dict.table"))
            Dict.table$new(data.table::rbindlist(list(self$values(),
                                                      other$values()), ...))
        },

        #' @description Rename a `key` in the `Dict`. An error is signaled, if
        #' either the `old` key is not in the `Dict` or the `new` key results
        #' in a name-clash with an existing key.
        #' @param old `character` name of key to be renamed.
        #' @param new `character` new key name.
        #' @return invisibly returns the `Dict.table` object
        rename = function(old, new) {
            #data.table::setnames(self$values(), old, new)
            if (!is.character(old)) stop("old must be character")
            if (!is.character(new)) stop("new must be character")
            super$rename(old, new)
        },

        #' @description Overrides `value` at `key` if `key` is already in the
        #' `Dict.table`. If `key` not in `Dict.table`, an error is thrown
        #' unless `add` was set to `TRUE`.
        #' @param key `character` name of key.
        #' @param value the value to be set
        #' @param add `logical` if `TRUE` the value is set regardless whether
        #' `key` already exists in `Dict.table`.
        #' @return invisibly returns the `Dict.table` object
        setval = function(key, value, add = FALSE) {
            if (!add) {
                if (!self$has(key)) {
                    stop("key '", key, "' not in ", data.class(self))
                }
            }

            data.table::set(self$values(), j = key, value = value)
            invisible(self)
        },

        #' @description Re-order columns according to key-order
        #' @param decr `logical` if `TRUE` sort in decreasing order.
        #' @return invisibly returns the `Dict.table`
        sortkey = function(decr = FALSE) {
            data.table::setcolorder(self$values(),
                                    sort(self$keys(), decreasing = decr))
            invisible(self)
        }
    ),
    private = list(nr = 0, row.names = character(0)),
    lock_class = TRUE
)

