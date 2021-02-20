#' A Set class
#'
#' @description The [Set()] is considered and implemented as a specialized
#' [Container()], that is, elements are always unique in the [Container()] and
#' it provides typical set operations such as `union` and `intersect`.
#' @author Roman Pahl
#' @seealso [Container()], [set()]
#' @export
Set <- R6::R6Class("Set",
    inherit = Container,
    public = list(
        #' @description `Set` constructor
        #' @param ... initial elements put into the `Set`
        #' @return invisibly returns the `Set`
        initialize = function(...) {

            private$elems = sets::set()

            for (elem in list(...))
                self$add(elem)

            invisible(self)
        },

        #' @description Add element
        #' @param elem If not already in set, add `elem`.
        #' @return invisibly returns [Set()] object.
        add = function(elem) {
            private$elems = sets::set_union(self$values(), elem)
            invisible(self)
        },

        #' @description Discard element from `Set` if it exists.
        #' @param elem element to be discarded.
        #' @return invisibly returns the `Set` object
        discard = function(elem) {
            private$elems = self$values() - sets::set(elem)
        },

        #' @description Determine if `Set` has some element.
        #' @param elem element to search for
        #' @return `TRUE` of `Set` contains `elem` else `FALSE`
        has = function(elem) {
            sets::cset_contains_element(self$values(), elem)
        },


        #' @description `Set` difference
        #' @param s `Set` object to 'subtract'
        #' @return new `Set` being the set difference between this and s.
        diff = function(s) {
            private$.verify_same_class(s)
            Set$new(self$values() - s$values())
        },

        #' @description `Set` intersection
        #' @param s `Set` object to 'intersect'
        #' @return new `Set` as a result of the intersection of this and s.
        intersect = function(s) {
            private$.verify_same_class(s)
            Set$new(sets::set_intersection(self$values(), s$values()))
        },

        #' @description `Set` union
        #' @param s `Set` object to be 'unified'
        #' @return new `Set` being the result of the union of this and s.
        union = function(s) {
            private$.verify_same_class(s)
            Set$new(sets::set_union(self$values(), s$values()))
        }
    ),
    lock_class = TRUE
)

