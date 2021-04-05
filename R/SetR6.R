#' Set
#'
#' @description The [Set()] is considered and implemented as a specialized
#' [Container()], that is, elements are always unique in the [Container()] and
#' it provides typical set operations such as `union` and `intersect`.
#' @seealso [Container()], [set()]
#' @export
Set <- R6::R6Class("Set",
    inherit = Container,
    public = list(
        #' @description `Set` constructor
        #' @param ... initial elements put into the `Set`
        #' @return returns the `Set` object
        initialize = function(...) {

            elems = list(...)
            super$initialize()

            if (length(elems))
                do.call(self$add, args = elems)

            self
        },

        #' @description Add element
        #' @param ... elements to be added to the `Set`
        #' @return the `Set` object.
        add = function(...) {
            elems = list(...)

            if (length(elems) == 0)
                return(self)

            if (length(elems) > 1) {
                for (i in seq_along(elems))
                    do.call(self$add, elems[i])

                return(self)
            }

            value = elems[[1]]
            if (self$has(value))
                return(self)

            named_value = elems[1]
            hash_value = private$get_hash_value(value)
            private$elems[[hash_value]] = named_value
            private$resort_by_hash()

            self
        },

        #' @description Search for occurence of `elem` in the `Set` and
        #' replace it by `new`. If `elem` does not exist, an error is
        #' signaled, unless `add` was set to `TRUE`, in which case `new` is
        #' added.
        #' @param old element to be replaced
        #' @param new element to be put instead of old
        #' @param add `logical` if `TRUE` the `new` element is added in case
        #' `old` does not exists.
        #' @return the `Set` object
        replace = function(old, new, add = FALSE) {
            if (add)
                self$discard(old)
            else
                self$delete(old)

            self$add(new)
            self
        },

        #' @description `Set` difference
        #' @param s `Set` object to 'subtract'
        #' @return the `Set` object updated as a result of the set difference
        #' between this and s.
        diff = function(s) {
            private$verify_same_class(s)
            lapply(s$values(), self$discard)
            self
        },

        #' @description `Set` intersection
        #' @param s `Set` object to 'intersect'
        #' @return the `Set` object as a result of the intersection of this and s.
        intersect = function(s) {
            private$verify_same_class(s)
            for (elem in self$values()) {
                if (!s$has(elem))
                    self$discard(elem)
            }
            self
        },

        #' @description `Set` union
        #' @param s `Set` object to be 'unified'
        #' @return the `Set` object as a result of the union of this and s.
        union = function(s) {
            private$verify_same_class(s)

            it = s$iter()
            while (it$has_next())
                do.call(self$add, args = it$get_next())

            self
        },

        #' @description `Set` equality
        #' @param s `Set` object to compare against
        #' @return `TRUE` if this is equal to `s`, otherwise `FALSE`
        is_equal = function(s) {
            private$verify_same_class(s)
            self == s
        },

        #' @description `Set` proper subset
        #' @param s `Set` object to compare against
        #' @return `TRUE` if this is subset of `s`, otherwise `FALSE`
        is_subset = function(s) {
            private$verify_same_class(s)
            self <= s
        },

        #' @description `Set` subset
        #' @param s `Set` object to compare against
        #' @return `TRUE` if this is proper subset of `s`, otherwise `FALSE`
        is_proper_subset = function(s) {
            private$verify_same_class(s)
            self < s
        },

        #' @description Get `Set` values
        #' @return elements of the set as a base list
        values = function() {
            l = private$elems
            names(l) = NULL
            if (length(l))
                unlist(l, recursive = FALSE)
            else
                l
        }
    ),
    private = list(
        deep_clone = function(name, value) {
            if (name != "elems")
                return(value)

            clone_deep_if_container = function(x) {
                elem = x[[1]]
                elem = if (is.container(elem))
                    elem$clone(deep = TRUE) else elem

                l = list(elem)
                names(l) = names(x)
                l
            }
            lapply(value, clone_deep_if_container)
        },
        get_hash_value = function(x) {
            lab = get_label(x)
            #x = unpack(as.list(x))
            #out = utils::capture.output(print(x))
            paste(length(x), lab, serialize(x, NULL), collapse = "")
        },
        resort_by_hash = function() {
            new_order = order(lengths(self$values()), names(private$elems))
            private$elems = private$elems[new_order]
        }
    ),
    lock_class = TRUE
)

