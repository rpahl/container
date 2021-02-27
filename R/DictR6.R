#' @title Dict
#'
#' @description The [Dict()] resembles Python's dict type, and is implemented
#' as a specialized associative [Container()] thus sharing all [Container()]
#' methods with some of them being overriden to account for the associative
#' key-value pair semantic.
#' @details Key-value pairs internally are stored in a hash-table.
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

            keys <- names(elems)
            keys.len <- length(keys)
            keys.nchars <- sapply(keys, nchar)
            if (length(elems) != keys.len || any(keys.nchars == 0))
                stop("all elems must be named")

            if (any(duplicated(keys)))
                stop("duplicated keys are not allowed for ", data.class(self))

            private$elems <- list2env(elems, parent = emptyenv(), hash = TRUE)
            invisible(self)
        },

        #' @description If `key` not yet in `Dict`, insert `value` at `key`,
        #' otherwise signal an error.
        #' @param key `character` name of key.
        #' @param value the value to be added to the `Dict`.
        add = function(key, value) {
            if (self$has(key))
                stop("key '", key, "' already in ", data.class(self))

            self$replace(key, value, add = TRUE)
        },

        #' @description If key in `Dict`, delete associated key-value pair.
        #' @param key `character` key of value to delete. If `key` does exist,
        #' the associated key-value pair is deleted, otherwise an error is
        #' signaled.
        #' @return invisibly returns the `Dict`
        delete = function(key) {
            if (!self$has(key))
                stop("key '", key, "' not in ", data.class(self))

            self$discard(key)
        },

        #' @description If key in `Dict`, discard associated key-value pair.
        #' @param key `character` key of value to discard. If `key` does exist,
        #' the associated key-value pair is deleted, otherwise it is ignored.
        #' @return invisibly returns the `Dict`
        discard = function(key) {
            if (self$has(key))
                remove(list = key, envir = private$elems)

            invisible(self)
        },

        #' @description This function is deprecated. Use [getvalue()] instead.
        #' @param key `character` name of key.
        #' @return If `key` in `Dict`, return value at `key`, else throw error.
        get = function(key) {
            .Deprecated("getvalue")
            self$getvalue(key)
        },

        #' @description Access value.
        #' @param key `character` name of key.
        #' @return If `key` in `Dict`, return value at `key`, else throw error.
        getvalue = function(key) {
            if (self$has(key))
                self$peek(key)
            else
                stop("key '", key, "' not in ", data.class(self))
        },

        #' @description Determine if `Dict` has a `key`.
        #' @param key `character` name of key.
        #' @return `TRUE` if `key` in `Dict`, otherwise `FALSE`.
        has = function(key) {
            if (length(key) != 1) stop("key must be of length 1")
            if (!is.character(key)) stop("key must be character")
            if (is.na(key)) stop("undefined key")
            if (isTRUE(nchar(key) == 0)) stop("zero-length key")
            utils::hasName(private$elems, key)
        },

        #' @description Get all keys.
        #' @return `character` vector of all keys.
        keys = function() {
            ls(envir = private$elems)
        },

        #' @description Peek for value in `Dict`.
        #' @param key `character` name of key.
        #' @param default returned default value.
        #' @return value for `key` if `key` is in the `Dict` else `default`.
        peek = function(key, default = NULL) {
            get0(key, envir = private$elems, ifnotfound = default)
        },

        #' @description peek random item
        #' @return returns an arbitrary element from the `Dict`. This
        #' function can be used to sample randomly (with replacement) from
        #' a `Dict`.
        peekitem = function() {
            if (self$empty())
                return(NULL)

            key <- sample(self$keys(), size = 1)
            self$peek(key)
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

        #' @description pop random item
        #' @return deletes and return an arbitrary element from the
        #' `Dict`. This function can be used to destructively iterate
        #'  over a `Dict`.
        popitem = function() {
            if (self$empty())
                stop("popitem at empty ", data.class(self))

            key <- sample(self$keys(), size = 1)
            self$pop(key)
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
        #' @return invisibly returns the `Dict` object
        rename = function(old, new) {
            if (length(old) != length(new))
                stop("old and new must be of same length")

            if (length(old) > 1) {
                mapply(self$rename, old, new)
                return(invisible(self))
            }

            if (identical(old, new))
                return(self)

            if (!self$has(old))
                stop("key '", old, "' not found")

            if (self$has(new))
                stop("rename failed because '", new, "' exists already")

            self$add(key = new, value = self$getvalue(old))
            self$delete(old)
            invisible(self)
        },

        #' @description Overrides `value` at `key` if `key` is already in the
        #' `Dict`. If `key` not in `Dict`, an error is thrown unless `add` was
        #' set to `TRUE`.
        #' @param key `character` name of key.
        #' @param value the value to be set
        #' @param add `logical` if `TRUE` the `value` is added in case
        #' `key` does not exists.
        #' @return invisibly returns the `Dict`
        replace = function(key, value, add = FALSE) {
            if (!add && !self$has(key))
                stop("key '", key, "' not in ", data.class(self))

            assign(key, value, envir = private$elems)
            invisible(self)
        },


        #' @description This function is deprecated. Use [replace()] instead.
        #' @param key `character` name of key.
        #' @param value the value to be set
        #' @param add `logical` if `TRUE` the value is set regardless whether
        #' `key` already exists in `Dict`.
        #' @return invisibly returns the `Dict`
        set = function(key, value, add = FALSE) {
            .Deprecated("replace")
            self$replace(key, value, add)
        },

        #' @description Sort elements according to their keys. This function
        #' is deprecated as keys are now always sorted.
        #' @param decr `logical` if `TRUE` sort in decreasing order.
        #' @return invisibly returns the `Dict`
        sort = function(decr = FALSE) {
            .Deprecated(msg = "'sort' is deprecated - keys are now always sorted")
            invisible(self)
        },

        #' @description Add elements of other dict to the `Dict` if the key is
        #' not in the `Dict` and update the key with the new value otherwise.
        #' @param other `Dict` dictionary used to update the `Dict`
        #' @return invisibly returns the `Dict`
        update = function(other = Dict$new()) {
            if (!inherits(other, data.class(self)))
                stop("arg must be a ", data.class(self))

            for (key in other$keys())
                self$replace(key, other$getvalue(key), add = TRUE)

            invisible(self)
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

