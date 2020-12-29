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
            elems <- list(...)
            if (nargs() == 1) elems <- elems[[1]]

            super$initialize(unique(elems))
            invisible(self)
        },

        #' @description Add element
        #' @param elem If not already in set, add `elem`.
        #' @return invisibly returns [Set()] object.
        add = function(elem) {
            type <- mode(self$values())
            if (length(elem) > 1 && type != "list") {
                it <- Iterator$new(elem)
                while(it$has_next()) {
                    self$add(it$get_next())
                }
                return(invisible(self))
            }

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
            diff <- base::setdiff(self$values(), s$values())
            Set$new(diff)
        },

        #' @description `Set` intersection
        #' @param s `Set` object to 'intersect'
        #' @return new `Set` as a result of the intersection of this and s.
        intersect = function(s) {
            if (!inherits(s, data.class(self))) {
                stop("s must be a ", data.class(self))
            }
            intersection <- base::intersect(self$values(), s$values())
            Set$new(intersection)
        },

        #' @description `Set` comparison
        #' @param s `Set` object to compare
        #' @return `TRUE` if `Set` s is equal to this, otherwise `FALSE`
        is.equal = function(s) {
            if (!inherits(s, data.class(self))) {
                stop("s must be a ", data.class(self))
            }
            setequal(self$values(), s$values())
        },

        #' @description Check if subset
        #' @param s `Set` object to check
        #' @return `TRUE` if `Set` s is subset of this, otherwise `FALSE`
        is.subset = function(s) {
            if (!inherits(s, data.class(self))) {
                stop("s must be a ", data.class(self))
            }
            length(base::setdiff(self$values(), s$values())) == 0
        },

        #' @description Check if superset
        #' @param s `Set` object to check
        #' @return `TRUE` if `Set` s is superset of this, otherwise `FALSE`
        is.superset = function(s) {
            if (!inherits(s, data.class(self))) {
                stop("s must be a ", data.class(self))
            }
            length(base::setdiff(s$values(), self$values())) == 0
        },

        #' @description `Set` union
        #' @param s `Set` object to 'unified'
        #' @return new `Set` being the result of the union of this and s.
        union = function(s) {
            if (!inherits(s, data.class(self))) {
                stop("s must be a ", data.class(self))
            }
            uniset <- base::union(self$values(), s$values())
            Set$new(uniset)
        }
    ),
    lock_class = TRUE
)

