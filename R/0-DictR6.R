#' @title Dict
#'
#' @description The [Dict()] resembles Python's dict type, and is implemented
#' as a specialized associative [Container()] thus sharing all [Container()]
#' methods with some of them being overriden to account for the associative
#' key-value pair semantic.
#' @details Internally, all key-value pairs are stored in a hash-table and the
#' elements are sorted lexicographically by their keys.
#' @importFrom R6 R6Class
#' @seealso [Container()], [dict()]
#' @export
Dict <- R6::R6Class("Dict",
    inherit = Container,
    public = list(
        #' @description `Dict` constructor
        #' @param ... initial elements put into the `Dict`
        #' @return returns the `Dict`
        initialize = function(...) {

            super$initialize()

            elems <- list(...)

            keys <- names(elems)
            keys.len <- length(keys)
            keys.nchars <- sapply(keys, nchar)
            if (length(elems) != keys.len || any(keys.nchars == 0))
                stop("all elements must be named", call. = FALSE)

            if (any(duplicated(keys)))
                stop("duplicated keys are not allowed", call. = FALSE)

            private$elems <- list2env(elems, parent = emptyenv(), hash = TRUE)
            self
        },

        #' @description If `key` not yet in `Dict`, insert `value` at `key`,
        #' otherwise signal an error.
        #' @param key `character` name of key.
        #' @param value the value to be added to the `Dict`.
        add = function(key, value) {
            if (self$has_name(key))
                stop("key '", key, "' already in ", data.class(self),
                     call. = FALSE)

            self$replace(key, value, add = TRUE)
        },


        #' @description If key in `Dict`, delete associated key-value pair.
        #' @param key `character` key of value to delete. If `key` does exist,
        #' the associated key-value pair is deleted, otherwise an error is
        #' signaled.
        #' @return the `Dict` object
        delete = function(key) {
            if (!self$has_name(key))
                stop("key '", key, "' not in ", data.class(self), call. = FALSE)

            self$discard(key)
        },

        #' @description If key in `Dict`, discard associated key-value pair.
        #' @param key `character` key of value to discard. If `key` does exist,
        #' the associated key-value pair is deleted, otherwise it is ignored.
        #' @return the `Dict`
        discard = function(key) {
            if (self$has_name(key))
                remove(list = key, envir = private$elems)

            self
        },

        #' @description This function is deprecated. Use [at2()] instead.
        #' @param key `character` name of key.
        #' @return If `key` in `Dict`, return value at `key`, else throw error.
        get = function(key) {
            .Deprecated("at")
            self$at2(key)
        },

        #' @description Get all keys.
        #' @return `character` vector of all keys.
        keys = function() {
            ls(envir = private$elems)
        },

        #' @description This function is deprecated. Use [delete()] instead.
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
        #' @return the `Dict` object
        rename = function(old, new) {
            .rename_check_names(self, old, new)

            if (length(old) > 1) {
                mapply(self$rename, old, new)
                return(self)
            }

            if (identical(old, new))
                return(self)

            self$add(key = new, value = self$at2(old))
            self$delete(old)
            self
        },

        #' @description Overrides `value` at `key` if `key` is already in the
        #' `Dict`. If `key` not in `Dict`, an error is thrown unless `add` was
        #' set to `TRUE`.
        #' @param key `character` name of key.
        #' @param value the value to be set
        #' @param add `logical` if `TRUE` the `value` is added in case
        #' `key` does not exists.
        #' @return returns the `Dict`
        replace = function(key, value, add = FALSE) {
            if (!add && !self$has_name(key))
                stop("key '", key, "' not in ", data.class(self), call. = FALSE)

            assign(key, value, envir = private$elems)
            self
        },


        #' @description This function is deprecated. Use [replace()] instead.
        #' @param key `character` name of key.
        #' @param value the value to be set
        #' @param add `logical` if `TRUE` the value is set regardless whether
        #' `key` already exists in `Dict`.
        #' @return returns the `Dict`
        set = function(key, value, add = FALSE) {
            .Deprecated("replace")
            self$replace(key, value, add)
        },

        #' @description Sort elements according to their keys. This function
        #' is deprecated as keys are now always sorted.
        #' @param decr `logical` if `TRUE` sort in decreasing order.
        #' @return returns the `Dict`
        sort = function(decr = FALSE) {
            .Deprecated(msg = "'sort' is deprecated - keys are now always sorted")
            self
        },

        #' @description Add elements of other dict to the `Dict` if the key is
        #' not in the `Dict` and update the key with the new value otherwise.
        #' @param other `Dict` dictionary used to update the `Dict`
        #' @return returns the `Dict`
        update = function(other = Dict$new()) {
            if (!inherits(other, data.class(self)))
                stop("arg must be a ", data.class(self), call. = FALSE)

            for (key in other$keys())
                self$replace(key, other$at2(key), add = TRUE)

            self
        },

        #' @description Get `Container` values
        #' @return a copy of all elements in a list
        values = function() as.list(private$elems)[self$keys()]
    ),
    private = list(
        # Since elems are stored in an environment for Dict, some extra care
        # has to be taken if a true/deep copy is desired.
        deep_clone = function(name, value) {
            if (name != "elems")
                return(value)

            clone_deep_if_container = function(x) {
                if (is.container(x))
                    x$clone(deep = TRUE) else x
            }

            l = as.list.environment(value, all.names = TRUE)
            list2env(lapply(l, clone_deep_if_container),
                     parent = emptyenv())
        }
    ),
    lock_class = TRUE,
)

