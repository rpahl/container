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
            private$elems <- c(private$elems, list(elem))
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
            pos <- private$.get_position(elem, right)
            if (!is.na(pos)) private$elems <- .subset(private$elems, -pos)
            invisible(self)
        },

        #' @description Check whether `Container` is empty
        #' @return `TRUE` if the `Container` is empty else `FALSE`
        empty = function() self$length() == 0,

        #' @description Determine if `Container` has some element.
        #' @param elem element to search for
        #' @return `TRUE` of `Container` contains `elem` else `FALSE`
        has = function(elem) {
            !is.na(private$.get_position(elem))
        },

        #' @description Number of elements of the `Container`.
        #' @return `integer` length of the `Container`, that is, the number of
        #' elements it contains.
        length = function() length(private$elems),

        #' @description peek random item
        #' @return returns an arbitrary element from the `Container`. This
        #' function can be used to sample randomly (with replacement) from
        #' a `Container`.
        peekitem = function() {
            if (self$empty()) {
                return(NULL)
            }
            pos <- sample(seq_along(private$elems), size = 1)
            .subset2(private$elems, pos)
        },

        #' @description pop random item
        #' @return deletes and return an arbitrary element from the
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

        #' @description Print object representation
        #' @param left `character` character printed as open bracket
        #' @param right `character` character printed as closing bracket
        #' @param len `integer` max number of elements per group
        #' @param ... further arguments passed to [format()]
        #' @return invisibly returns the `Container` object
        print = function(left = "[", right = "]", len = 6L, ...) {
            cat(LABEL(self, limit = 0), "\n")
            x = .format_values(self$values(),
                               left = left, right = right,
                               limit = len, ...)

            writeLines(strwrap(format(x, ...), exdent = 1L))
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

        #' @description This function is deprecated and of no real use anymore.
        #' @return type (or mode) of internal vector containing the elements
        type = function() {
            old = as.character(sys.call(sys.parent()))[1L]
            object <- strsplit(old, split = "$", fixed = TRUE)[[1]][1]
            msg <- paste0("'", old, "()' is deprecated and not useful anymore.\n",
                          "You can use 'mode(", object, "$values())' instead.")
            .Deprecated("mode", msg = msg)
            mode(private$elems)
        },

        #' @description Get `Container` values
        #' @return a copy of all elements in a list
        values = function() private$elems
    ),
    private = list(
        elems = list(),
        create_iter = function() Iterator$new(self$values()),
        .create_compare_fun = function(x) {
            function(y) isTRUE(all.equal(x, y))
        },
        .get_position = function(elem, right = FALSE) {
            Position(f = private$.create_compare_fun(elem),
                     x = private$elems,
                     right = right)
        }
    ),
    lock_class=TRUE
)


.create_object_string <- function(x, x.names, name_seps, ...)
{
    if (length(x) == 0) return("")

    paste(x.names, name_seps, LABELS(as.list(x), ...),
          sep = "", collapse = ", ")
}

.format_values <- function(x, left, right, ...)
{
    x.names <- names(x)
    names(x) <- NULL
    name_seps <- rep.int("", length(x))

    if (!is.null(x.names)) {
        name_seps[x.names != ""] <- " = "
    }

    obj_str <- .create_object_string(x, x.names, name_seps)

    paste0(left, obj_str, right)
}


format.Container <- function(x, ...)
{
    .format_values(values(x), left = "[", right = "]", ...)
}

format.Set <- function(x, ...)
{
    .format_values(values(x), left = "{", right = "}", ...)
}

format.Dict <- function(x, ...)
{
    .format_values(values(x), left = "{", right = "}", ...)
}

