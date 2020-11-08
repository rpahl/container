#' @title A sequence container
#'
#' @description This class implements a container data structure with typical
#' member functions to insert, delete and access objects from the container.
#' While it can be used to create [Container()] objects, it mainly serves as the
#' base class for [Deque()], [Set()], and [Dict()].
#' @details
#' The underlying data structure is based on R vectors (or lists), with the mode
#' being set to the mode (or type) of the value passed to the initialize
#' function, which by default is an empty list, in which case the
#' [Container()] object can store objects of mixed and arbitrary types.
#' If the container will only contain objects of one particular type, for
#' example, numeric values, it will be both more efficient and type safe to
#' initialize the container with this particular type.
#' @author Roman Pahl
#' @docType class
#' @importFrom R6 R6Class
#' @seealso [Iterable()], [Deque()], [Set()], and [Dict()]
#' @export
Container <- R6::R6Class("Container",
    inherit = container:::Iterable,
    public = list(
        #' @description constructor
        #' @param x initial elements put into the `Container`
        #' @return invisibly returns the `Container` object
        initialize = function(x = list()) {
            private$elems <- as.vector(x)
            names(private$elems) <- names(x)
            stopifnot(is.vector(private$elems))
            attr(self, "name") <- paste0("<", data.class(self), ">")
            attr(self, "class") <- unique(c(attr(self, "class"), "Container"))
            invisible(self)
        },

        #' @description add element
        #' @param elem element to be added to `Container` object
        #' @return invisibly returns the `Container` object
        add = function(elem) {
            if (inherits(elem, "Container")) {
                lapply(elem$values(), self$add)
            } else {
                if (self$type() == "list") {
                    private$elems <- c(private$elems, list(elem))
                } else {
                    v <- Reduce(f=c, x=elem, init=private$elems)
                    private$elems <- as.vector(v, mode=self$type())
                }
            }
            invisible(self)
        },

        #' @description apply function to all `Container` elements
        #' @param f `function` to apply
        #' @return `list` of results retrieved from the applied function
        apply = function(f) {
            if (!is.function(f)) stop("f must be a function")
            lapply(private$elems, FUN = f)
        },

        #' @description Remove all elements from the `Container`
        #' @return invisibly returns the cleared `Container` object
        clear = function() {
            self$initialize(vector(typeof(private$elems)))
        },

        #' @description Search for first `elem` in `Container` and, if found,
        #' remove it. If not found, the `Container` object is not altered.
        #' @param elem element to be discarded.
        #' @param right `logical` if `TRUE`, search from right to left.
        #' @return invisibly returns the `Container` object
        discard = function(elem, right = FALSE) {
            comp <- function(x) isTRUE(all.equal(x, elem))
            pos <- Position(f=comp, x=private$elems, right=right, nomatch=0)
            if (pos > 0) private$elems <- private$elems[-pos]
            invisible(self)
        },

        #' @description Check whether `Container` is empty
        #' @return `TRUE` if the `Container` is empty else `FALSE`
        empty = function() self$size() == 0,

        #' @description Determine if `Container` has some element.
        #' @param elem element to search for
        #' @return `TRUE` of `Container` contains `elem` else `FALSE`
        has = function(elem) {
            comp <- function(x) isTRUE(all.equal(x, elem))
            any(sapply(private$elems, FUN=comp))
        },

        #' @description Print object representation similar to [utils::str()]
        #' @param list.len `integer` maximum number of elements to display
        #' @param ... other arguments passed to [utils::str()]
        #' @return invisibly returns the `Container` object
        print = function(list.len = 10L, ...) {
            cat0 <- function(...) cat(..., sep="")
            class_name <- paste0("<", data.class(self), ">")

            cat0(class_name, " of ", self$size(), " elements: ")
            utils::str(self$values(), list.len = list.len, ...)
            if (list.len < self$size()) {
                cat0("... with ", self$size() - list.len, " more elements")
            }
            invisible(self)
        },

        #' @description Find and remove element from `Container`
        #' @param elem element to be removed from the `Container`. If element
        #'  is not found in the `Container`, an error is signaled.
        #' @param right `logical` if `TRUE`, search from right to left.
        #' @return invisibly returns the `Container` object
        remove = function(elem, right = FALSE) {
            class <- data.class(self)
            hasElem <- self$has(elem)
            if (hasElem) {
                self$discard(elem, right)
            } else {
                stop(elem, " not in ", class)
            }
            invisible(self)
        },

        #' @description Size of the `Container`
        #' @return the `Container` size
        size = function() length(private$elems),

        #' @description Underlying data type
        #' @return type (or mode) of internal vector containing the elements
        type = function() typeof(private$elems),

        #' @description Get copy of `Container` values
        #' @return a copy of all elements in the same format as they are stored
        #' in the `Container` object.
        values = function() private$elems
    ),
    private = list(elems = vector(mode = "list"),
        create_iter = function() Iterator$new(private$elems)
    ),
    lock_class=TRUE
)

