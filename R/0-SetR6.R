#' Set class
#'
#' @description The [Set()] is considered and implemented as a specialized
#' [Container()], that is, elements are always unique in the [Container()] and
#' it provides typical set operations such as `union` and `intersect`.
#' @details Under the hood, elements of a set object are stored in a hash-table
#' and sorted by their length and, in case of ties, by their lexicographical
#' representation.
#' @seealso [Container()], [set()]
#' @export
Set <- R6::R6Class("Set",
    inherit = Container,
    public = list(
        #' @description `Set` constructor
        #' @param ... initial elements put into the `Set`
        #' @return returns the `Set` object
        initialize = function(...) {

            super$initialize()

            it = iter(list(...), .subset = .subset)
            while (has_next(it)) {
                value = get_next(it)
                self$add(value[[1]], name = names(value))
            }

            self
        },

        #' @description Add element
        #' @param value value of `ANY` type to be added to the `Set`.
        #' @param name `character` optional name attribute of the value.
        #' @return the `Set` object.
        add = function(value, name = NULL) {

            if (self$has(value))
                return(self)

            elem = list(value)
            names(elem) = name

            hash_value = private$.get_hash_value(value)
            private$elems[[hash_value]] = elem
            private$.reorder_values()

            self
        },

        #' @description `Set` difference
        #' @param s `Set` object to 'subtract'
        #' @return the `Set` object updated as a result of the set difference
        #' between this and s.
        diff = function(s) {
            private$.verify_same_class(s)
            lapply(s$values(), self$discard)
            self
        },

        #' @description `Set` intersection
        #' @param s `Set` object to 'intersect'
        #' @return the `Set` object as a result of the intersection of this and s.
        intersect = function(s) {
            private$.verify_same_class(s)
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
            private$.verify_same_class(s)

            it = s$iter()

            while (has_next(it)) {
                value = get_next(it)
                self$add(value[[1]], name = names(value))
            }

            self
        },

        #' @description `Set` equality
        #' @param s `Set` object to compare against
        #' @return `TRUE` if this is equal to `s`, otherwise `FALSE`
        is_equal = function(s) {
            private$.verify_same_class(s)
            self == s
        },

        #' @description `Set` proper subset
        #' @param s `Set` object to compare against
        #' @return `TRUE` if this is subset of `s`, otherwise `FALSE`
        is_subset = function(s) {
            private$.verify_same_class(s)
            self <= s
        },

        #' @description `Set` subset
        #' @param s `Set` object to compare against
        #' @return `TRUE` if this is proper subset of `s`, otherwise `FALSE`
        is_proper_subset = function(s) {
            private$.verify_same_class(s)
            self < s
        },

        #' @description Get `Set` values
        #' @return elements of the set as a base list
        values = function() {
            l = private$elems
            names(l) = NULL
            res = if (length(l))
                unlist(l, recursive = FALSE)
            else
                l

            if (!any(nzchar(names(res))))
                names(res) <- NULL

            res
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

        .get_hash_value = function(x) {
            tmp = tempfile()
            on.exit(file.remove(tmp))
            saveRDS(x, tmp)
            tools::md5sum(tmp)
        },

        .rename = function(old, new) {
            pos = match(old, names(self))
            names(private$elems[[pos]]) = new
        },

        .replace_value_at = function(pos, value, name) {
            self$discard_at(pos)
            self$add(value, name)
            self
        },

        .reorder_values = function() {
            new_order = order(lengths(self$values()),
                              sapply(self$values(), .get_label),
                              names(private$elems))
            private$elems = private$elems[new_order]
        }
    ),
    lock_class = TRUE
)

