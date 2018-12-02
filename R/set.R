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
#' @section R6 constructor:
#' \code{Set$new(x=list())}
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
        initialize = function(x=list()) super$initialize(unique(x)),
        add = function(elem) {},
        union = function(s) {},
        intersect = function(s) {},
        diff = function(s) {},
        is.equal = function(s) setequal(self$values(), s$values()),
        is.subset = function(s) {},
        is.superset = function(s) {}
    )
)


# Set method implementations
Set$set("public", "add", overwrite=TRUE,
    function(elem) {
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
    }
)

Set$set("public", "union", overwrite=TRUE,
    function(s) {
        if (!inherits(s, "Set")) stop("s must be a Set")
        union <- base::union(self$values(), s$values())
        Set$new(union)
    }
)

Set$set("public", "intersect", overwrite=TRUE,
    function(s) {
        if (!inherits(s, "Set")) stop("s must be a Set")
        intersection <- base::intersect(self$values(), s$values())
        Set$new(intersection)
    }
)

Set$set("public", "diff", overwrite=TRUE,
    function(s) {
        if (!inherits(s, "Set")) stop("s must be a Set")
        diff <- base::setdiff(self$values(), s$values())
        Set$new(diff)
    }
)

Set$set("public", "is.subset", overwrite=TRUE,
    function(s) {
        length(base::setdiff(self$values(), s$values())) == 0
    }
)

Set$set("public", "is.superset", overwrite=TRUE,
    function(s) {
        length(base::setdiff(s$values(), self$values())) == 0
    }
)
Set$lock()


