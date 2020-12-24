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
            invisible(it)
        }
    ),
    private = list(create_iter = function() stop("abstract method")),
    lock_class=TRUE
)


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
    inherit = Iterable,
    public = list(
        #' @description constructor
        #' @param ... initial elements put into the `Container`
        #' @param keep_names `logical` if TRUE, keeps names of passed elements
        #' @return invisibly returns the `Container` object
        initialize = function(..., keep_names = FALSE) {
            args <- list(...)
            n.elems <- nargs() - !missing(keep_names)
            elems <- if (n.elems == 1) args[[1]] else args

            if (!is.vector(elems)) elems <- list(elems)

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
            if (self$type() == "list") {
                elem <- list(elem)
            } else {
                if (self$type() != mode(elem)) {
                    stop("type mismatch: expected '", self$type(),
                         "' but got '", mode(elem), "'")
                }
            }
            private$elems <- c(private$elems, elem)
            invisible(self)
        },

        #' @description delete all elements from the `Container`
        #' @return invisibly returns the cleared `Container` object
        clear = function() {
            self$initialize(vector(mode(private$elems)))
        },

        #' @description Find and delete element from `Container`
        #' @param elem element to be deleted from the `Container`. If element
        #'  is not found in the `Container`, an error is signaled.
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
            comp <- function(x) isTRUE(all.equal(x, elem))
            pos <- Position(f = comp,
                            x = private$elems,
                            right = right,
                            nomatch = 0)
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
            !is.na(Position(f = comp, x = private$elems))
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

        #' @description Find and remove element from `Container`. This
        #' function does the same as `delete` and is only kept for backwards
        #' compatibility.
        #' @param elem element to be deleted from the `Container`. If element
        #'  is not found in the `Container`, an error is signaled.
        #' @param right `logical` if `TRUE`, search from right to left.
        #' @return invisibly returns the `Container` object
        remove = function(elem, right = FALSE) {
            self$delete(elem, right)
        },

        #' @description Size of the `Container`
        #' @return the `Container` size
        size = function() length(private$elems),

        #' @description Underlying data type
        #' @return type (or mode) of internal vector containing the elements
        type = function() mode(private$elems),

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

