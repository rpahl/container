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

        #' @description If `name` not yet in `Dict`, insert `value` at `name`,
        #' otherwise signal an error.
        #' @param name `character` variable name under which to store `value`.
        #' @param value the value to be added to the `Dict`.
        #' @return the `Dict` object
        add = function(name, value) {
            force(value)
            if (self$has_name(name))
                stop("name '", name, "' already in ", data.class(self),
                     call. = FALSE)

            assign(name, value, envir = private$elems)
            self
        },

        #' @description Discard value at given index. If index is not found,
        #' the operation is ignored.
        #' @param index `character` or `numeric` index
        #' @return the `Dict` object
        discard_at = function(index) {

            pos = private$.get_index_position(index)

            if (has_index(self, pos))
                base::remove(list = self$keys()[pos], envir = private$elems)

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

            self$add(name = new, value = self$at2(old))
            self$delete(old)
            self
        },

        #' @description Replace one element by another element.
        #' Search for occurence of `old` and, if found, replace it by `new`.
        #' If `old` does not exist, an error is signaled.
        #' @param old element to be replaced
        #' @param new element to be put instead of old
        #' @return the `Dict` object
        replace = function(old, new) {

            pos = private$.get_element_position(old)
            force(new)

            hasElem = !is.na(pos)
            if (!hasElem)
                stop("old element (", get_label(old),
                     ") is not in ", data.class(self), call. = FALSE)

            name = names(self)[[pos]]
            private$.replace_value_at(pos, new, name)
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
            self$replace_at(key, value, add)
        },

        #' @description Sort elements according to their keys. This function
        #' is deprecated as keys are now always sorted.
        #' @param decr `logical` if `TRUE` sort in decreasing order.
        #' @return returns the `Dict`
        sort = function(decr = FALSE) {
            .Deprecated(msg = "'sort' is deprecated - keys are now always sorted")
            self
        },

        #' @description Add elements of `other` to this if the name is
        #' not in the `Dict` and update elements with existing names.
        #' @param other `Iterable` object used to update this.
        #' @return returns the updated `Dict` object.
        update = function(other) {

            if (length(other) != sum(nzchar(names(other))))
                stop("all elements of 'other' must be named")

            super$update(other)
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
        },

        .replace_value_at = function(pos, value, name) {
            assign(name, value, envir = private$elems)
        }

    ),
    lock_class = TRUE,
)

