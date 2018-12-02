#' @title Set constructors 
#' @description The \code{set} is considered and implemented as a specialized
#'  \code{\link[container]{container}} in which elements are always unique. It
#'  provides typical set operations such as \code{union (+)} and
#'  \code{intersect (/)}.
#' @author Roman Pahl
#' @name setS3
#' @seealso \code{\link[container]{container}}, \code{\link[container]{Set}},
#'  \code{\link[container]{+.Set}},
#'  \code{\link[container]{/.Set}},
#'  \code{\link[container]{-.Set}},
#'  \code{\link[container]{<.Set}},
#'  \code{\link[container]{>.Set}}
#' 
#' @param x (vector or list) initial elements of the set
#' @export set as.set is.set
#'
#' @section S3 methods for class \code{Set}:
#' \describe{
#'  \item{\code{add(x, elem)}}{If not already in set \code{x}, add \code{elem}.}
#' }
#' 
#' @examples
#' s1 <- set(list(1, 2, "A", "B"))
#' s2 <- set(values(s1))
#' s1 == s2     # TRUE
#' s1$add(1)    # 1 was already in set, therefore ...
#' s1 == s2     # ... still TRUE
#' s1$add(3)
#' s1 == s2     # FALSE
#' s1 > s2      # TRUE
#' s1 - s2      # the added element
#' unlist(values(s1 / s2))
#'
#' \dontrun{
#' s1$remove(4)              # Error: 3 not in Set
#' }
NULL

#' @rdname setS3
set <- function(x=list()) Set$new(x)

#' @rdname setS3
as.set <- function(x) Set$new(x)

#' @rdname setS3
is.set <- function(x) inherits(x, "Set")

#' @export
add.Set <- function(x, elem, ...) x$add(elem)


#' @title Binary set operators
#' @description Binary operators for \code{Set} objects.
#' @name setS3binOp
#' @param s1 \code{\link[container]{Set}} object
#' @param s2 \code{\link[container]{Set}} object
NULL

#' @rdname setS3binOp
#' @return union of both sets
#' @export
`+.Set` <- function(s1, s2) s1$union(s2)

#' @rdname setS3binOp
#' @return intersection of both sets
#' @export
`/.Set` <- function(s1, s2) s1$intersect(s2)

#' @rdname setS3binOp
#' @return set-difference of both sets
#' @export
`-.Set` <- function(s1, s2) s1$diff(s2)

#' @rdname setS3binOp
#' @return \code{TRUE} if both sets are equal, else \code{FALSE}
#' @export
`==.Set` <- function(s1, s2) s1$is.equal(s2)

#' @rdname setS3binOp
#' @return \code{TRUE} if s1 is subset of s2, else \code{FALSE}
#' @export
`<.Set` <- function(s1, s2) s1$is.subset(s2)

#' @rdname setS3binOp
#' @return \code{TRUE} if s1 is superset of s2, else \code{FALSE}
#' @export
`>.Set` <- function(s1, s2) s1$is.superset(s2)

