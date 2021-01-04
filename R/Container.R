#' Iterable abstract class interface
#'
#' @description An `Iterable` is an object that provides an `iter` method,
#' which is expected to return an `Iterator` object. This class defines the
#' abstract class interface such that each class inheriting this class provides
#' an [iter()] method and must implement a private method `create_iter`,
#' which must return an `Iterator` object.
#' @author Roman Pahl
#' @docType class
#' @importFrom R6 R6Class
#' @seealso `Iterator` and `Container`
Iterable <- R6::R6Class("Iterable",
    public = list(

        #' @description
        #' `Iterable` is an abstract class and thus cannot be instantiated.
        initialize = function() stop("abstract class"),

        #' @description Create iterator
        #' @return invisibly returns the `Iterator` object.
        iter = function() {
            it <- private$create_iter()
            hasIterator <- inherits(it, "Iterator")
            if (!hasIterator) stop("Iterable did not create an Iterator")
            it
        }
    ),
    private = list(create_iter = function() stop("abstract method")),
    lock_class=TRUE
)


#' @title A sequence Container
#'
#' @description This class implements a container data structure with typical
#' member functions to insert, delete and access elements from the container.
#' While it can be used to create [Container()] objects, it mainly serves as the
#' base class for [Deque()], [Set()], and [Dict()].
#' @details
#' The underlying data structure is based on R vectors (or lists), with the mode
#' being set to the mode (or type) of the value passed to the initialize
#' function, which by default is an empty list, in which case the
#' [Container()] object can store elements of mixed and arbitrary types.
#' If the container will only contain atomic elements of one particular type,
#' for example, numeric values, it will be both more efficient and type safe to
#' initialize the container with this particular type.
#' @author Roman Pahl
#' @docType class
#' @importFrom R6 R6Class
#' @seealso [Iterable()], [Deque()], [Set()], and [Dict()]
#' @export
Container <- R6::R6Class("Container",
    inherit = Iterable,
    public = list(
        #' @description constructor
        #' @param ... initial elements put into the `Container`
        #' @param keep_names `logical` if TRUE, keeps names of passed elements
        #' @return invisibly returns the `Container` object
        initialize = function(..., keep_names = FALSE) {
            elems <- list(...)

            if (!keep_names) {
                names(elems) <- NULL
            }

            private$elems <- elems
            invisible(self)
        },

        #' @description add element
        #' @param elem element to be added to `Container` object
        #' @return invisibly returns the `Container` object
        add = function(elem) {
            type <- mode(self$values())
            if (type == "list") {
                elem <- list(elem)
            } else {
                if (type != mode(elem)) {
                    stop("type mismatch: expected '", type,
                         "' but got '", mode(elem), "'")
                }
            }
            private$elems <- c(private$elems, elem)
            invisible(self)
        },

        #' @description delete all elements from the `Container`
        #' @return invisibly returns the cleared `Container` object
        clear = function() {
            self$initialize()
        },

        #' @description Find and delete element from `Container`
        #' @param elem element to be deleted from the `Container`. If not
        #' found, an error is signaled.
        #' @param right `logical` if `TRUE`, search from right to left.
        #' @return invisibly returns the `Container` object
        delete = function(elem, right = FALSE) {
            if (self$has(elem)) {
                self$discard(elem, right)
            } else {
                stop(elem, " not in ", data.class(self))
            }
            invisible(self)
        },

        #' @description Search for first `elem` in `Container` and, if found,
        #' delete it. If not found, the `Container` object is not altered.
        #' @param elem element to be discarded.
        #' @param right `logical` if `TRUE`, search from right to left.
        #' @return invisibly returns the `Container` object
        discard = function(elem, right = FALSE) {
            #comp <- function(x) isTRUE(all.equal(x, elem))
            comp <- function(x) identical(x, elem)
            pos <- Position(f = comp,
                            x = private$elems,
                            right = right,
                            nomatch = 0)
            if (pos > 0) private$elems <- .subset(private$elems, -pos)
            invisible(self)
        },

        #' @description Check whether `Container` is empty
        #' @return `TRUE` if the `Container` is empty else `FALSE`
        empty = function() self$length() == 0,

        #' @description Determine if `Container` has some element.
        #' @param elem element to search for
        #' @return `TRUE` of `Container` contains `elem` else `FALSE`
        has = function(elem) {
            #comp <- function(x) isTRUE(all.equal(x, elem))
            comp <- function(x) identical(x, elem)
            !is.na(Position(f = comp, x = private$elems))
        },

        #' @description Number of elements of the `Container`.
        #' @return `integer` length of the `Container`
        length = function() length(private$elems),

        #' @description delete and return an arbitrary element from the
        #' `Container`. This function can be used to destructively iterate
        #'  over a `Container` as often used in set algorithms.
        popitem = function() {
            if (self$empty()) {
                stop("popitem at empty ", data.class(self))
            }
            pos <- sample(seq_along(private$elems), size = 1)
            elem <- .subset2(private$elems, pos)
            private$elems <- .subset(private$elems, -pos)
            elem
        },

        #' @description return an arbitrary element from the `Container`.
        #' This function can be used to sample randomly (with replacement)
        #' from a `Container`.
        peekitem = function() {
            if (self$empty()) {
                return(NULL)
            }
            pos <- sample(seq_along(private$elems), size = 1)
            .subset2(private$elems, pos)
        },

        #' @description Print object representation similar to [utils::str()]
        #' @param len `numeric` maximum number of elements to display
        #' @param ... other arguments passed to [utils::str()]
        #' @return invisibly returns the `Container` object
        print = function(len, ...) {
            if (missing(len)) len <- strOptions()[["list.len"]]
            cat0 <- function(...) cat(..., sep="")
            class_name <- paste0("<", data.class(self), ">")

            elem_str <- if (self$length() == 1) "element" else "elements"
            cat(class_name, "of", self$length(), elem_str)
            if (self$length()) {
                cat0(":\n")
                utils::str(self$values(),
                           list.len = len,
                           no.list = TRUE,
                           comp.str = "", ...)
                if (len < self$length()) {
                    cat0("... with ", self$length() - len, " more elements")
                }
            } else {
                cat("\n")
            }
            invisible(self)
        },

        #' @description This function is deprecated. Use [delete()] instead.
        #' @param elem element to be deleted from the `Container`. If element
        #'  is not found in the `Container`, an error is signaled.
        #' @param right `logical` if `TRUE`, search from right to left.
        #' @return invisibly returns the `Container` object
        remove = function(elem, right = FALSE) {
            .Deprecated("delete")
            self$delete(elem, right)
        },

        #' @description This function is deprecated. Use [length()] instead.
        #' @return the `Container` length
        size = function() {
            .Deprecated("length")
            length(private$elems)
        },

        #' @description This function is deprecated.
        #' @return type (or mode) of internal vector containing the elements
        type = function() {
            old = as.character(sys.call(sys.parent()))[1L]
            object <- strsplit(old, split = "$", fixed = TRUE)[[1]][1]
            msg <- paste0("'", old, "()' is deprecated.\n",
                          "Use 'mode(", object, "$values())' instead.")
            .Deprecated("mode", msg = msg)
            mode(private$elems)
        },

        #' @description Get copy of `Container` values
        #' @return a copy of all elements in a list
        #' otherwise as a basic `list`.
        values = function() private$elems
    ),
    private = list(elems = list(),
                   create_iter = function() Iterator$new(self$values())
    ),
    lock_class=TRUE
)

