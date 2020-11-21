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
        #' @param x initial elements put into the `Dict`
        #' @return invisibly returns the `Dict`
        initialize = function(x = list()) {
            if (is.data.frame(x)) x <- as.list(x)
            name_len <- sapply(names(x), nchar)
            if (length(x) != length(name_len) || any(name_len == 0)) {
                stop("all items must be named")
            }
            super$initialize(x)
            if (any(duplicated(self$keys()))) stop("duplicated keys")
            invisible(self)
        },

        #' @description If `key` not yet in `Dict`, insert `value` at `key`,
        #' otherwise signal an error.
        #' @param key `character` name of key.
        #' @param value the value to be added to the `Dict`.
        add = function(key, value) {
            if (self$has(key)) {
                stop("key '", key, "' already in ", data.class(self))
            }
            self$set(key, value, add = TRUE)
        },

        #' @description If key in `Dict`, remove it.
        #' @param key `character` key of value to discard
        #' @return invisibly returns the `Dict`
        discard = function(key) {
            if (self$has(key)) {
                pos <- match(key, self$keys())
                private$elems <- private$elems[-pos]
            }
            invisible(self)
        },

        #' @description Access value.
        #' @param key `character` name of key.
        #' @return If `key` in `Dict`, return value at `key`, else throw error.
        get = function(key) {
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
            if (nchar(key) == 0) stop("zero-length key")
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
            if (self$has(key)) private$elems[[key]] else default
        },

        #' @description Get value and remove key-value pair from `Dict`.
        #' it afterwards. If `key` not found, raise an error.
        #' @param key `character` name of key.
        #' @return If `key` in `Dict`, return its value.
        pop = function(key) {
            elem <- self$peek(key)
            self$remove(key)
            elem
        },

        #' @description Remove and return an arbitrary (key, value) pair
        #'  from the `Dict`. This function can be used to destructively iterate
        #'  over a `Dict` as often used in set algorithms.
        #' @return random key-value pair from the `Dict`
        popitem = function() {
            if (self$empty()) {
                stop("pop at empty ", data.class(self))
            } else {
                key <- sample(self$keys(), 1)
                key_value_pair <- private$elems[key]
                self$remove(key)
                return(key_value_pair)
            }
        },

        #' @description Remove value
        #' @param key `character` name of key.
        #' @return If `key` in `Dict`, remove it, otherwise raise an error.
        remove = function(key) {
            if (!self$has(key)) {
                stop("key '", key, "' not in ", data.class(self))
            }
            self$discard(key)
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

        #' @description Overrides `value` at `key` if `key` is already in the
        #' `Dict`. If `key` not in `Dict`, an error is thrown unless `add` was
        #' set to `TRUE`.
        #' @param key `character` name of key.
        #' @param value the value to be set
        #' @param add `logical` if `TRUE` the value is set regardless whether
        #' `key` already exists in `Dict`.
        #' @return invisibly returns the `Dict`
        set = function(key, value, add = FALSE) {
            if (!add) {
                if (!self$has(key)) stop("key '", key, "' not in ",
                                         data.class(self))
            }
            private$elems[[key]] <- value
            invisible(self)
        },

        #' @description Re-order elements according to key-order
        #' @param decr `logical` if `TRUE` sort in decreasing order.
        #' @return invisibly returns the `Dict`
        sort = function(decr = FALSE) {
            private$elems <- private$elems[order(self$keys(), decreasing=decr)]
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
                self$set(key, other$get(key), add = TRUE)
            }
            invisible(self)
        }
    ),
    lock_class = TRUE
)

