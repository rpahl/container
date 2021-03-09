SETS_MATCHFUN <- sets::matchfun(function(x, y) isTRUE(all.equal(x, y)))
#SETS_MATCHFUN <- sets::matchfun(function(x, y) identical(x, y))

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
        #' @return invisibly returns the `Set`
        initialize = function(...) {

            super$initialize(...)

            sets::sets_options("matchfun", SETS_MATCHFUN)
            private$elems = sets::as.cset(self$values())
            invisible(self)
        },

        #' @description Add element
        #' @param elem If not already in set, add `elem`.
        #' @return invisibly returns [Set()] object.
        add = function(elem) {
            private$elems = sets::cset_union(self$values(), elem)
            invisible(self)
        },

        #' @description Discard element from `Set` if it exists.
        #' @param elem element to be discarded.
        #' @return invisibly returns the `Set` object
        discard = function(elem) {
            private$elems = self$values() - sets::cset(elem)
        },

        #' @description Determine if `Set` has some element.
        #' @param elem element to search for
        #' @return `TRUE` of `Set` contains `elem` else `FALSE`
        has = function(elem) {
            sets::cset_contains_element(self$values(), elem)
        },

        #' @description Search for occurence of `elem` in the `Set` and
        #' replace it by `new`. If `elem` does not exist, an error is
        #' signaled, unless `add` was set to `TRUE`, in which case `new` is
        #' added.
        #' @param old element to be replaced
        #' @param new element to be put instead of old
        #' @param add `logical` if `TRUE` the `new` element is added in case
        #' `old` does not exists.
        #' @return invisibly returns the `Set` object
        replace = function(old, new, add = FALSE) {
            if (add)
                self$discard(old)
            else
                self$delete(old)

            self$add(new)
            invisible(self)
        },

        #' @description `Set` difference
        #' @param s `Set` object to 'subtract'
        #' @return new `Set` being the set difference between this and s.
        diff = function(s) {
            private$.verify_same_class(s)
            as.set(as.list(self$values() - s$values()))
        },

        #' @description `Set` intersection
        #' @param s `Set` object to 'intersect'
        #' @return new `Set` as a result of the intersection of this and s.
        intersect = function(s) {
            private$.verify_same_class(s)
            intersection = sets::cset_intersection(self$values(), s$values())
            as.set(as.list(intersection))
        },

        #' @description `Set` union
        #' @param s `Set` object to be 'unified'
        #' @return new `Set` being the result of the union of this and s.
        union = function(s) {
            private$.verify_same_class(s)
            the_union = sets::cset_union(self$values(), s$values())
            as.set(as.list(the_union))
        },

        #' @description `Set` equality
        #' @param s `Set` object to compare against
        #' @return `TRUE` if this is equal to `s`, otherwise `FALSE`
        is_equal = function(s) {
            private$.verify_same_class(s)
            length(self) == length(s) &&
                sets::cset_is_equal(self$values(), s$values())
        },

        #' @description `Set` proper subset
        #' @param s `Set` object to compare against
        #' @return `TRUE` if this is subset of `s`, otherwise `FALSE`
        is_subset = function(s) {
            private$.verify_same_class(s)
            length(self) <= length(s) && self$diff(s)$empty()
        },

        #' @description `Set` subset
        #' @param s `Set` object to compare against
        #' @return `TRUE` if this is proper subset of `s`, otherwise `FALSE`
        is_proper_subset = function(s) {
            private$.verify_same_class(s)
            length(self) < length(s) && self$diff(s)$empty()
        }
    ),
    lock_class = TRUE
)

