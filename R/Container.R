#' @title A sequence container
#' @description This class implements a container data structure with typical
#' member functions to insert, delete and access objects from the container. It
#' also serves as the base class for \code{\link[container]{Deque}},
#' \code{\link[container]{Set}}, and \code{\link[container]{Dict}}.
#' @details
#' The underlying data structure is based on R vectors (or lists), with the mode
#' being set to the mode (or type) of the value passed to the initialize 
#' function, which by default is an empty list, in which case the 
#' \code{Container} object can store objects of mixed and arbitrary types.
#' If the container will only contain objects of one particular type, for
#' example, double values, it will be both more efficient and type safe to
#' initialize the container using this particular type (see Examples section).
#' @author Roman Pahl
#' @docType class
#' @importFrom R6 R6Class
#' @seealso \code{\link[container]{Iterable}}, \code{\link[container]{Deque}},
#' \code{\link[container]{Set}}, and \code{\link[container]{Dict}}
#'
#' @section Container methods:
#' \describe{
#'  \item{\code{add(elem)}}{Add \code{elem} to \code{Container}.}
#'  \item{\code{apply(f)}}{Apply function f to all elements and return results
#'  in a list)}
#'  \item{\code{clear()}}{Remove all elements from the \code{Container}.}
#'  \item{\code{discard(elem, right=FALSE)}}{Search for first \code{elem} in
#'      \code{Container} and, if found, remove it. If \code{right} is
#'      \code{TRUE}, search from right to left.}
#'  \item{\code{empty()}}{Return \code{TRUE} if the \code{Container} is empty,
#'      else \code{FALSE}.}
#'  \item{\code{has(elem)}}{Return \code{TRUE} if \code{Container} contains
#'      \code{elem} else \code{FALSE}.}
#'  \item{\code{print(list.len)}}{Print object representation similar to
#'      \code{\link[utils]{str}}}
#'  \item{\code{remove(elem, right=FALSE)}}{Same as \code{discard}, but throw an
#'      error if not found.}
#'  \item{\code{size()}}{Return size of the \code{Container}.}
#'  \item{\code{type()}}{Return type (or mode) of internal vector containing
#'  the elements.}
#'  \item{\code{values()}}{Return a copy of all elements in the same format
#'  as they are stored in the object.}
#' }
#' @examples
#' c0 <- Container$new()
#' c0$size()                            # 0
#' c0$add(1)
#' c0$add(2)$add("A")                   # chaining example
#' c0$has(2)                            # TRUE
#' c0$discard(2)$has(2)                 # FALSE
#'
#' \dontrun{
#' c0$remove(2)                         # Error : 2 not in Container
#' }
#' c0$discard(2)$has(2)                 # still FALSE, but no error
#'
#' # Container types
#' Container$new(list("A", 1))$type()   # "list"
#' Container$new(numeric(0))$type()     # "double"
#' Container$new(0+0i)$type()           # "complex"
#' Container$new(letters[1:3])$type()   # "character"
#' Container$new(letters[1:3])$values() # "a" "b" "c"
#' Container$new(1L)$type()             # "integer"
#' Container$new(1L)$add(2.3)$values()  # since integer type, equals c(1, 2)
#' @export
Container <- R6::R6Class("Container",
    inherit = container:::Iterable,
    public = list(
        initialize = function(x=list()) {},
        add = function(elem) {},
        apply = function(f) {},
        clear = function() self$initialize(vector(typeof(private$elems))),
        discard = function(elem, right=FALSE) {},
        empty = function() self$size() == 0,
        has = function(elem) {},
        print = function(list.len=10L, ...) {},
        remove = function(elem, right=FALSE) {},
        size = function() length(private$elems),
        type = function() typeof(private$elems),
        values = function() private$elems
    ),
    private = list(elems = vector(mode="list"),
        create_iter = function() Iterator$new(private$elems)
    ),
)


# Container method implementations
Container$set("public", "initialize", overwrite=TRUE,
    function(x=list()) {
        private$elems <- as.vector(x)
        names(private$elems) <- names(x)
        stopifnot(is.vector(private$elems))
        attr(self, "name") <- paste0("<", data.class(self), ">")
        invisible(self)
    }
)

Container$set("public", "add", overwrite=TRUE,
    function(elem) {
        if (self$type() == "list") {
            private$elems <- c(private$elems, list(elem))
        } else {
            v <- Reduce(f=c, x=elem, init=private$elems)
            private$elems <- as.vector(v, mode=self$type())
        }
        invisible(self)
    }
)

Container$set("public", "apply", overwrite=TRUE,
    function(f) {
        if (!is.function(f)) stop("f must be a function")
        lapply(private$elems, FUN=f)
    }
)

Container$set("public", "discard", overwrite=TRUE,
    function(elem, right=FALSE) {
        comp <- function(x) isTRUE(all.equal(x, elem))
        pos <- Position(f=comp, x=private$elems, right=right, nomatch=0)
        if (pos > 0) private$elems <- private$elems[-pos]
        invisible(self)
    }
)

Container$set("public", "has", overwrite=TRUE,
    function(elem) {
        comp <- function(x) isTRUE(all.equal(x, elem))
        any(sapply(private$elems, FUN=comp))
    }
)

Container$set("public", "print", overwrite=TRUE,
    function(list.len=10, ...) {
        cat0 <- function(...) cat(..., sep="")
        class_name <- paste0("<", data.class(self), ">")

        cat0(class_name, " of ", self$size(), " elements: ")
        utils::str(self$values(), list.len=list.len, ...)
        if (list.len < self$size()) {
            cat0("... with ", self$size() - list.len, " more elements")
        }
        invisible(self)
    }
)

Container$set("public", "remove", overwrite=TRUE,
    function(elem, right=FALSE) {
        class <- data.class(self)
        hasElem <- self$has(elem)
        if (hasElem) self$discard(elem, right) else stop(elem, " not in ", class)
        invisible(self)
    }
)
Container$lock()
