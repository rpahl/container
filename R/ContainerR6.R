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
        #' @return invisibly returns the `Container` object
        initialize = function(...) {
            private$elems <- list(...)
            invisible(self)
        },

        #' @description add element
        #' @param x element to be added to `Container` object
        #' @return invisibly returns the `Container` object
        add = function(x) {
            private$elems <- c(private$elems, list(x))
            invisible(self)
        },

        #' @description delete all elements from the `Container`
        #' @return invisibly returns the cleared `Container` object
        clear = function() {
            self$initialize()
        },

        #' @description Count number of element occurences.
        #' @param x element to be counted.
        #' @return `integer` number of `x` occurences in the [Container()]
        count = function(x) {
            sum(sapply(private$elems, FUN = private$get_compare_fun(x)))
        },

        #' @description Search for occurence(s) of `x` in `Container` and
        #' remove all of them. If `x` does not exist, an error is signaled.
        #' @param x element to be removed from the `Container`.
        #' @return invisibly returns the `Container` object
        delete = function(x) {
            elem_str = deparse(substitute(x))
            if (!self$has(x))
                stop(elem_str, " is not in ", data.class(self))

            self$discard(x)
        },

        #' @description Search for occurence(s) of `x` in `Container` and
        #' remove all of them.
        #' @param x element to be discarded from the `Container`. If not
        #' found, the operation is ignored and the is object *not* altered.
        #' @return invisibly returns the `Container` object
        discard = function(x) {

            f = Negate(private$get_compare_fun(x))
            private$elems = Filter(f, private$elems)

            invisible(self)
        },

        #' @description Check whether `Container` is empty
        #' @return `TRUE` if the `Container` is empty else `FALSE`
        empty = function() self$length() == 0,

        #' @description Determine if `Container` has some element.
        #' @param x element to search for
        #' @return `TRUE` of `Container` contains `x` else `FALSE`
        has = function(x) {
            !is.na(private$.get_position(x))
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
            x <- .subset2(private$elems, pos)
            private$elems <- .subset(private$elems, -pos)
            x
        },

        #' @description Print object representation
        #' @param len `integer` max number of elements per group
        #' @param ... further arguments passed to [format()]
        #' @return invisibly returns the `Container` object
        print = function(len = 6L, ...) {
            cat(LABEL(self, limit = 0), "\n", sep = "")
            x = format(self, limit = len, ...)

            writeLines(strwrap(format(x, ...), exdent = 1L))
            invisible(self)
        },

        #' @description Replace one element by another element

        #' @description Search for occurence(s) of `x` in `Container` and
        #' replace them by `y`. If `x` does not exist, an error is
        #' signaled, unless `add` was set to `TRUE`, in which case `y` is
        #' added.
        #' @param x element to be replaced
        #' @param y element to be put instead of x
        #' @param add `logical` if `TRUE` the `y` element is added in case
        #' `x` does not exists.
        #' @return invisibly returns the `Container` object
        replace = function(x, y, add = FALSE) {

            is_matching_old = private$get_compare_fun(x)

            pos = integer(0)
            if (!self$empty())
                pos = which(sapply(self$values(), is_matching_old))

            hasElem = length(pos) > 0
            if (!hasElem && add)
                return(self$add(y))

            if (!hasElem)
                stop(deparse(substitute(x)), " is not in ", data.class(self))

            new_elem = if (length(y)) y else list(y)
            private$elems <- replace(self$values(), pos, new_elem)
            invisible(self)
        },

        #' @description This function is deprecated. Use [delete()] instead.
        #' @param x element to be deleted from the `Container`. If element
        #'  is not found in the `Container`, an error is signaled.
        #' @return invisibly returns the `Container` object
        remove = function(x) {
            .Deprecated("delete")
            self$delete(x)
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
        #' @return elements of the container as a base list
        values = function() private$elems
    ),
    private = list(
        elems = list(),
        create_iter = function() Iterator$new(self$values()),
        deep_clone = function(name, value) {
            if (name != "elems") return(value)

            clone_deep_if_container = function(x) {
                if (inherits(x, "Container")) x$clone(deep = TRUE) else x
            }
            lapply(value, clone_deep_if_container)
        },
        get_compare_fun = function(x) {
            function(y) isTRUE(all.equal(x, y))
        },
        .get_position = function(x, right = TRUE, ...) {
            Position(f = private$get_compare_fun(x),
                     x = private$elems,
                     right = right,
                     ...)
        },
        .verify_same_class = function(x) {
            if (!inherits(x, data.class(self))) {
                stop("arg must be a ", data.class(self))
            }
        }
    ),
    lock_class = TRUE
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

    if (!is.null(x.names))
        name_seps[x.names != ""] <- " = "

    obj_str = .create_object_string(x, x.names, name_seps)

    paste0(left, obj_str, right)
}


format.Container <- function(x, ...)
{
    .format_values(values(x), left = "[", right = "]", ...)
}

format.Dict <- function(x, ...)
{
    .format_values(values(x), left = "[", right = "]", ...)
}

format.Deque <- function(x, ...)
{
    .format_values(values(x), left = "|", right = "|", ...)
}

format.Set <- function(x, ...)
{
    .format_values(values(x), left = "{", right = "}", ...)
}

