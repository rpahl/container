#' @title A Set class
#' @description The \code{Set} is considered and implemented as a specialized
#' \code{\link[container]{Container}}, that is, elements are always unique in the
#' \code{\link[container]{Container}} and it provides typical set operations such as
#' \code{union} and \code{intersect}.
#' @author Roman Pahl
#' @docType class
#' @importFrom R6 R6Class
#' @seealso \code{\link[container]{Container}}
#'
#' @section Inherited methods:
#' Inherits all methods from \code{\link[container]{Container}}, but overrides
#' \code{add}:
#' \describe{
#'  \item{\code{add(elem)}}{If not already in set, add \code{elem}.}
#' }
#'
#' @section Set methods:
#' \describe{
#'  \item{\code{union(s)}}{Return new \code{Set} as a result of the union of
#'  this and s.}
#'  \item{\code{intersect(s)}}{Return new \code{Set} as a result of the
#'      intersection of this and s.}
#'  \item{\code{diff(s)}}{Return new \code{Set} as a result of the set
#'      difference between this and s.}
#'  \item{\code{is.subset(s)}}{TRUE if this is a subset of s, else FALSE.}
#'  \item{\code{is.superset(s)}}{TRUE if this is a superset of s, else FALSE.}
#' }
#' @examples
#' s1 <- Set$new()$add("A")
#' s1$values()                     # "A"
#' s1$add(2)$add("A")$values()     # "A" 2
#' s1$remove("A")$values()         # 2
#'
#' #' \dontrun{
#' #' s1$remove(3)              # Error: 3 not in Set
#' #' }
#' @export
Set <- R6::R6Class("Set",
    inherit = Container,
    public = list(
        initialize = function(x=list()) {
            super$initialize(unique(x))
        },
        add = function(elem) {
            if (self$type() == "list") {
                if (!self$has(elem)) super$add(elem)
            } else {
                if (length(elem) > 1) {
                    # Perform element-wise adding to catch duplicates
                    lapply(elem, FUN=function(x) self$add(x))
                } else {
                    if (!self$has(elem)) super$add(elem)
                }
            }
            invisible(self)
        },
        union = function(s) {
            if (!inherits(s, "Set")) stop("s must be a Set")
            union <- base::union(self$values(), s$values())
            Set$new(union)
        },
        intersect = function(s) {
            if (!inherits(s, "Set")) stop("s must be a Set")
            intersection <- base::intersect(self$values(), s$values())
            Set$new(intersection)
        },
        diff = function(s) {
            if (!inherits(s, "Set")) stop("s must be a Set")
            diff <- base::setdiff(self$values(), s$values())
            Set$new(diff)
        },
        is.equal = function(s) {
            base::setequal(self$values(), s$values())
        },
        is.subset = function(s) {
            length(base::setdiff(self$values(), s$values())) == 0
        },
        is.superset = function(s) {
            length(base::setdiff(s$values(), self$values())) == 0
        }
    ),
    lock_class=TRUE
)

