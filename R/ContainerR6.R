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
        initialize = function() stop("abstract class", call. = FALSE),

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
            f.cmp = container_options("compare")[[1]]
            private$set_compare_fun(f.cmp)
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

        #' @description Count number of element occurences.
        #' @param elem element to be counted.
        #' @return `integer` number of `elem` occurences in the [Container()]
        count = function(elem) {
            sum(sapply(private$elems, FUN = private$compare_predicate(elem)))
        },

        #' @description Search for occurence(s) of `elem` in `Container` and
        #' remove first one that is found. If `elem` does not exist, an error
        #' is signaled.
        #' @param elem element to be removed from the `Container`.
        #' @return invisibly returns the `Container` object
        delete = function(elem) {
            elem_str = deparse(substitute(elem))
            if (!self$has(elem))
                stop(elem_str, " is not in ", data.class(self))

            self$discard(elem)
        },

        #' @description Search for occurence(s) of `elem` in `Container` and
        #' remove first one that is found.
        #' @param elem element to be discarded from the `Container`. If not
        #' found, the operation is ignored and the object is *not* altered.
        #' @return invisibly returns the `Container` object
        discard = function(elem) {
            if (self$empty())
                return(invisible(self))

            pos = private$get_position(elem)

            hasElem = !is.na(pos)
            if (hasElem)
                private$elems <- .subset(private$elems, -pos)

            invisible(self)
        },

        #' @description Check whether `Container` is empty
        #' @return `TRUE` if the `Container` is empty else `FALSE`
        empty = function() self$length() == 0,

        #' @description Get comparison function used internally by the
        #' `Container` object to compare elements.
        get_compare_fun = function() private$compare_fun,

        #' @description Determine if `Container` has some element.
        #' @param elem element to search for
        #' @return `TRUE` if `Container` contains `elem` else `FALSE`
        has = function(elem) {
            !is.na(private$get_position(elem))
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
        #' @param ... further arguments passed to [format()]
        #' @return invisibly returns the `Container` object
        print = function(...) {
            vec.len = container_options("vec.len")[[1]]
            useDots = container_options("useDots")[[1]]
            x = format(self, vec.len = vec.len, useDots = useDots, ...)

            writeLines(strwrap(format(x, ...), exdent = 1L))
            invisible(self)
        },

        #' @description Replace one element by another element.
        #' Search for occurence of `old` in `Container` and, if found,
        #' replace it by `new`. If `old` does not exist, an error is
        #' signaled, unless `add` was set to `TRUE`, in which case `new` is
        #' added.
        #' @param old element to be replaced
        #' @param new element to be put instead of old
        #' @param add `logical` if `TRUE` the `new` element is added in case
        #' `old` does not exists.
        #' @return invisibly returns the `Container` object
        replace = function(old, new, add = FALSE) {

            pos = private$get_position(old)

            hasElem = !is.na(pos)
            if (!hasElem && add)
                return(self$add(new))

            if (!hasElem)
                stop(deparse(substitute(old)), " is not in ", data.class(self))

            new_elem = if (length(new)) new else list(new)
            private$elems <- replace(self$values(), pos, new_elem)
            invisible(self)
        },

        #' @description This function is deprecated. Use [delete()] instead.
        #' @param elem element to be deleted from the `Container`. If element
        #'  is not found in the `Container`, an error is signaled.
        #' @return invisibly returns the `Container` object
        remove = function(elem) {
            .Deprecated("delete")
            self$delete(elem)
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
        compare_fun = NULL,
        elems = list(),
        create_iter = function() Iterator$new(self, private$.subset),
        deep_clone = function(name, value) {
            if (name != "elems")
                return(value)

            clone_deep_if_container = function(x) {
                if (is.container(x))
                    x$clone(deep = TRUE) else x
            }
            lapply(value, clone_deep_if_container)
        },
        compare_predicate = function(x) {
            function(y) isTRUE(private$compare_fun(x, y))
        },
        get_position = function(x, right = TRUE, ...) {
            Position(f = private$compare_predicate(x),
                     x = self$values(),
                     right = right,
                     ...)
        },
        set_compare_fun = function(x) {
            f = if (is.character(x)) match.fun(x) else x
            private$compare_fun = f
        },
        verify_same_class = function(x) {
            if (!inherits(x, data.class(self))) {
                stop("arg must be a ", data.class(self))
            }
        },
        .subset = function(x, ...) .subset(x$values(), ...)
    ),
    lock_class = TRUE
)

