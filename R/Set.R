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
            super$initialize()
            lapply(list(...), self$add)
            invisible(self)
        },

        #' @description Add element
        #' @param elem If not already in set, add `elem`.
        #' @return invisibly returns [Set()] object.
        add = function(elem) {
            if (!self$has(elem)) {
                super$add(elem)
            }
            invisible(self)
        },

        #' @description `Set` difference
        #' @param s `Set` object to 'subtract'
        #' @return new `Set` being the set difference between this and s.
        diff = function(s) {
            if (!inherits(s, data.class(self))) {
                stop("s must be a ", data.class(self))
            }

            # Starting from a copy of self, discard all elems that are in s
            res <- self$clone()
            it <- Iterator$new(s$values())
            while(it$has_next()) {
                elem <- it$get_next()
                res$discard(elem)
            }
            res
        },

        #' @description `Set` intersection
        #' @param s `Set` object to 'intersect'
        #' @return new `Set` as a result of the intersection of this and s.
        intersect = function(s) {
            if (!inherits(s, data.class(self))) {
                stop("s must be a ", data.class(self))
            }

            # Starting from new empty set, add all values that are in both
            res <- Set$new()
            it <- Iterator$new(self$values())
            while(it$has_next()) {
                elem <- it$get_next()
                if (s$has(elem)) {
                    res$add(elem)
                }
            }
            res
        },

        #' @description `Set` comparison
        #' @param s `Set` object to compare
        #' @return `TRUE` if this is equal to `s`, otherwise `FALSE`
        is.equal = function(s) {
            if (!inherits(s, data.class(self))) {
                stop("s must be a ", data.class(self))
            }

            # If unequal length we can stop right here
            if (self$length() != s$length()) return(FALSE)

            # Since set is not sorted, we have to check each element.
            it <- Iterator$new(self$values())
            while(it$has_next()) {
                elem <- it$get_next()
                if (!s$has(elem)) {
                    return(FALSE)
                }
            }
            TRUE
        },

        #' @description Check if subset
        #' @param s `Set` object to check
        #' @param s `logical` strict subset (<) or just subset (<=)?
        #' @return `TRUE` if this is subset of `s`, otherwise `FALSE`
        is.subset = function(s, strict = TRUE) {
            if (!inherits(s, data.class(self))) {
                stop("s must be a ", data.class(self))
            }

            # Start with the easy case
            if (self$length() > s$length()) return(FALSE)

            # Since set is not sorted, we have to check each element.
            it <- Iterator$new(self$values())
            while(it$has_next()) {
                elem <- it$get_next()
                if (!s$has(elem)) {
                    return(FALSE)
                }
            }

            # If we get here, we have either equality or strict subset
            if (self$length() == s$length() && strict) FALSE else TRUE
        },

        #' @description Check if superset
        #' @param s `Set` object to check
        #' @param s `logical` strict superset (>) or just superset (>=)?
        #' @return `TRUE` if this is superset of `s`, otherwise `FALSE`
        is.superset = function(s, strict = TRUE) {
            if (!inherits(s, data.class(self))) {
                stop("s must be a ", data.class(self))
            }
            s$is.subset(self, strict = strict)
        },

        #' @description `Set` union
        #' @param s `Set` object to 'unified'
        #' @return new `Set` being the result of the union of this and s.
        union = function(s) {
            if (!inherits(s, data.class(self))) {
                stop("s must be a ", data.class(self))
            }

            # Add all values from s to a copy of self
            res <- self$clone()
            it <- Iterator$new(s$values())
            while(it$has_next()) {
                elem <- it$get_next()
                res$add(elem)
            }
            res
        }
    ),
    lock_class = TRUE
)

