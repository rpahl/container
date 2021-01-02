#' @title A Dict class
#'
#' @description The [Dict()] resembles Python's dict type, and is implemented
#' as a specialized associative [Container()] thus sharing all [Container()]
#' methods with some of them being overriden to account for the associative
#' key-value pair semantic.
#'
#' @author Roman Pahl
#' @importFrom R6 R6Class
#' @seealso [Container()], [dict()]
#' @export
Dict <- R6::R6Class("Dict",
    inherit = Container,
    public = list(
        #' @description `Dict` constructor
        #' @param ... initial elements put into the `Dict`
        #' @return invisibly returns the `Dict`
        initialize = function(...) {
            elems <- list(...)
            if (nargs() == 1 && is.null(names(elems))) {
                elems <- elems[[1]]
            }

            keys <- names(elems)
            keys.len <- length(keys)
            keys.nchars <- sapply(keys, nchar)
            if (length(elems) != keys.len || any(keys.nchars == 0)) {
                stop("all elems must be named")
            }

            if (any(duplicated(keys))) {
                stop("duplicated keys")
            }
            super$initialize(elems, keep_names = TRUE)
        },

        #' @description If `key` not yet in `Dict`, insert `value` at `key`,
        #' otherwise signal an error.
        #' @param key `character` name of key.
        #' @param value the value to be added to the `Dict`.
        add = function(key, value) {
            if (self$has(key)) {
                stop("key '", key, "' already in ", data.class(self))
            }
            self$setval(key, value, add = TRUE)
        },

        #' @description If key in `Dict`, delete associated key-value pair.
        #' @param key `character` key of value to delete. If `key` does exist,
        #' the associated key-value pair is deleted, otherwise an error is
        #' signaled.
        #' @return invisibly returns the `Dict`
        delete = function(key) {
            if (!self$has(key)) {
                stop("key '", key, "' not in ", data.class(self))
            }
            self$discard(key)
        },

        #' @description If key in `Dict`, discard associated key-value pair.
        #' @param key `character` key of value to discard. If `key` does exist,
        #' the associated key-value pair is deleted, otherwise it is ignored.
        #' @return invisibly returns the `Dict`
        discard = function(key) {
            if (self$has(key)) {
                pos <- match(key, self$keys())
                private$elems <- .subset(private$elems, -pos)
            }
            invisible(self)
        },

        #' @description This function is deprecated. Use `getval` instead.
        #' @param key `character` name of key.
        #' @return If `key` in `Dict`, return value at `key`, else throw error.
        get = function(key) {
            .Deprecated("getval")
            self$getval(key)
        },

        #' @description Access value.
        #' @param key `character` name of key.
        #' @return If `key` in `Dict`, return value at `key`, else throw error.
        getval = function(key) {
            if (self$has(key)) {
                self$peek(key)
            } else {
                stop("key '", key, "' not in ", data.class(self))
            }
        },

        #' @description Determine if `Dict` has a `key`.
        #' @param key `character` name of key.
        #' @return `TRUE` if `key` in `Dict`, otherwise `FALSE`.
        has = function(key) {
            if (length(key) != 1) stop("key must be of length 1")
            if (!is.character(key)) stop("key must be character")
            if (is.na(key)) stop("undefined key")
            if (isTRUE(nchar(key) == 0)) stop("zero-length key")
            key %in% self$keys()
        },

        #' @description Get all keys.
        #' @return `character` vector of all keys.
        keys = function() {
            as.character(names(private$elems))
        },

        #' @description Peek for value in `Dict`.
        #' @param key `character` name of key.
        #' @param default returned default value.
        #' @return value for `key` if `key` is in the `Dict` else `default`.
        peek = function(key, default = NULL) {
            if (self$has(key)) .subset2(private$elems, key) else default
        },

        #' @description Get value and delete key-value pair from `Dict`.
        #' If `key` not found, raise an error.
        #' @param key `character` name of key.
        #' @return If `key` in `Dict`, return its value.
        pop = function(key) {
            elem <- self$peek(key)
            self$delete(key)
            elem
        },

        #' @description This function is deprecated. Use `delete` instead.
        #' @param key `character` name of key.
        #' @return If `key` in `Dict`, remove it, otherwise raise an error.
        remove = function(key) {
            .Deprecated("delete")
            self$delete(key)
        },

        #' @description Rename a `key` in the `Dict`. An error is signaled, if
        #' either the `old` key is not in the `Dict` or the `new` key results
        #' in a name-clash with an existing key.
        #' @param old `character` name of key to be renamed.
        #' @param new `character` new key name.
        #' @return invisibly returns the `Dict` object
        rename = function(old, new) {
            if (length(old) != length(new)) {
                stop("old and new must be of same length")
            }
            if (length(old) > 1) {
                for (i in seq_along(old)) {
                    self$rename(old[i], new[i])
                }
                return(invisible(self))
            }

            if (identical(old, new)) return(self)
            if (!self$has(old)) stop("key '", old, "' not found")
            if (self$has(new)) {
                stop("rename failed because '", new, "' exists already")
            }
            pos = match(old, self$keys())
            names(private$elems)[pos] <- new
            invisible(self)
        },

        #' @description This function is deprecated. Use `setval` instead.
        #' @param key `character` name of key.
        #' @param value the value to be set
        #' @param add `logical` if `TRUE` the value is set regardless whether
        #' `key` already exists in `Dict`.
        #' @return invisibly returns the `Dict`
        set = function(key, value, add = FALSE) {
            .Deprecated("setval")
            self$setval(key, value, add)
        },

        #' @description Overrides `value` at `key` if `key` is already in the
        #' `Dict`. If `key` not in `Dict`, an error is thrown unless `add` was
        #' set to `TRUE`.
        #' @param key `character` name of key.
        #' @param value the value to be set
        #' @param add `logical` if `TRUE` the value is set regardless whether
        #' `key` already exists in `Dict`.
        #' @return invisibly returns the `Dict`
        setval = function(key, value, add = FALSE) {
            if (!add && !self$has(key)) {
                stop("key '", key, "' not in ", data.class(self))
            }
            if (length(value) == 0) {
                private$elems[key] <- list(value)
            } else {
                private$elems[[key]] <- value
            }
            invisible(self)
        },

        #' @description This function is deprecated. Use `sortkey` instead.
        #' @param decr `logical` if `TRUE` sort in decreasing order.
        #' @return invisibly returns the `Dict`
        sort = function(decr = FALSE) {
            .Deprecated("sortkey")
            self$sortkey(decr)
        },

        #' @description Re-order elements according to key-order
        #' @param decr `logical` if `TRUE` sort in decreasing order.
        #' @return invisibly returns the `Dict`
        sortkey = function(decreasing = FALSE) {
            new_order <- order(self$keys(), decreasing = decreasing)
            private$elems <- .subset(private$elems, new_order)
            invisible(self)
        },

        #' @description Add elements of other dict to the `Dict` if the key is
        #' not in the `Dict` and update the key with the new value otherwise.
        #' @param other `Dict` dictionary used to update the `Dict`
        #' @return invisibly returns the `Dict`
        update = function(other = Dict$new()) {
            if (!inherits(other, data.class(self))) {
                stop("arg must be a ", data.class(self))
            }
            for (key in other$keys()) {
                self$setval(key, other$getval(key), add = TRUE)
            }
            invisible(self)
        }
    ),
    lock_class = TRUE
)

