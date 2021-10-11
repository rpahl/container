#' Iterable abstract class interface
#'
#' @description An [Iterable] is an object that provides an [iter()] method,
#' which is expected to return an [Iterator] object. This class defines the
#' abstract class interface such that each class inheriting this class provides
#' an [iter()] method and must implement a private method `create_iter()`,
#' which must return an [Iterator] object.
#' @author Roman Pahl
#' @docType class
#' @importFrom R6 R6Class
#' @seealso [Iterator] and [Container]
Iterable <- R6::R6Class("Iterable",
    public = list(

        #' @description
        #' `Iterable` is an abstract class and thus cannot be instantiated.
        initialize = function() stop("abstract class", call. = FALSE),

        #' @description Create iterator
        #' @return returns the `Iterator` object.
        iter = function() {
            it <- private$create_iter()
            hasIterator <- inherits(it, "Iterator")
            if (!hasIterator)
                stop("Iterable did not create an Iterator", call. = FALSE)
            it
        }
    ),
    private = list(create_iter = function()
        stop("abstract method", call. = FALSE)),

    lock_class=TRUE
)



#' @title Container Class
#'
#' @description This class implements a container data structure with typical
#' member functions to insert, delete and access elements from the container.
#' For the standard S3 interface, see [container()].
#' @details This class inherits from class [Iterable] and serves as the base
#' class for [Deque], [Set], and [Dict].
#' @author Roman Pahl
#' @docType class
#' @importFrom R6 R6Class
#' @seealso [container()], [Iterable], [Deque], [Set], and [Dict]
#' @export
#' @examples
#' co <- Container$new(1:5, c = Container$new("a", 1), l = list(1:3))
#' co$print()
#' co$length()
#'
#' co$add(3)
Container <- R6::R6Class("Container",
    inherit = Iterable,
    public = list(
        #' @description constructor
        #' @param ... initial elements put into the `Container`
        #' @return the `Container` object
        initialize = function(...) {
            private$elems <- list(...)
            f.cmp = container_options("compare")[[1]]
            private$.set_compare_fun(f.cmp)
            self
        },

        #' @description add element
        #' @param value value of `ANY` type to be added to the `Container`.
        #' @param name `character` optional name attribute of the value.
        #' @return the `Container` object
        add = function(value, name = NULL) {

            elem = list(value)
            names(elem) = name

            private$elems <- c(private$elems, elem)
            self
        },

        #' @description Same as `at2` (see below) but accepts a vector of
        #' indices and always returns a `Container` object.
        #' @param index vector of indices.
        #' @return `Container` object with the extracted elements.
        at = function(index) {

            if (missing(index))
                stop("'index' is missing", call. = FALSE)

            lapply(index, .assert_index_and_arg, x = self)

            l = lapply(index, function(x) private$.subset(self, x))
            if (!length(l))
                return(methods::as(l, data.class(self)))

            ul = unlist(l, recursive = FALSE)
            methods::as(ul, data.class(self))
        },

        #' @description Extract value at index. If index is invalid or not
        #' found, an error is signaled. If given as a string, the element
        #' matching the name is returned. If there are two or more identical
        #' names, the value of the first match (i.e. *leftmost* element) is
        #' returned.
        #' @param index Must be a single number > 0 or a string.
        #' @return If given as a number, the element at the corresponding
        #' position, and if given as a string, the element at the
        #' corresponding name matching the given string is returned.
        at2 = function(index) {

            if (missing(index))
                stop("'index' is missing", call. = FALSE)

            .assert_index_and_arg(self, index)
            private$.subset2(self, index)
        },

        #' @description delete all elements from the `Container`
        #' @return the cleared `Container` object
        clear = function() {
            self$initialize()
        },

        #' @description Count number of element occurences.
        #' @param elem element to be counted.
        #' @return `integer` number of `elem` occurences in the [Container()]
        count = function(elem) {
            if (self$is_empty())
                return(0)

            sum(sapply(private$elems, FUN = private$compare_predicate(elem)))
        },

        #' @description Search for occurence(s) of `elem` in `Container` and
        #' remove first one that is found. If `elem` does not exist, an error
        #' is signaled.
        #' @param elem element to be removed from the `Container`.
        #' @return the `Container` object
        delete = function(elem) {
            if (!self$has(elem))
                stop(.get_label(elem), " is not in ", data.class(self),
                     call. = FALSE)

            self$discard(elem)
        },

        #' @description Delete value at given index. If index is not found, an
        #' error is signaled.
        #' @param index `character` or `numeric` index
        #' @return the `Container` object
        delete_at = function(index) {
            .assert_index_and_arg(self, index)

            self$discard_at(index)
        },

        #' @description Search for occurence(s) of `elem` in `Container` and
        #' remove first one that is found.
        #' @param elem element to be discarded from the `Container`. If not
        #' found, the operation is ignored and the object is *not* altered.
        #' @return the `Container` object
        discard = function(elem) {

            pos = private$.get_element_position(elem, nomatch = 0)

            self$discard_at(pos)
        },

        #' @description Discard value at given index. If index is not found,
        #' the operation is ignored.
        #' @param index `character` or `numeric` index
        #' @return the `Container` object
        discard_at = function(index) {

            pos = private$.get_index_position(index)

            if (.assert_and_has_index(self, pos))
                private$elems <- .subset(private$elems, -pos)

            self
        },

        #' @description This function is deprecated. Use [is_empty()] instead.
        empty = function() {
            .Deprecated("is_empty")
            self$is_empty()
        },

        #' @description Get comparison function used internally by the
        #' `Container` object to compare elements.
        get_compare_fun = function() private$compare_fun,

        #' @description Determine if `Container` has some element.
        #' @param elem element to search for
        #' @return `TRUE` if `Container` contains `elem` else `FALSE`
        has = function(elem) {
            !is.na(private$.get_element_position(elem))
        },

        #' @description Determine if `Container` object contains an element
        #' with the given name. If called with no argument, the function
        #' determines whether *any* element is named.
        #' @param name `character` the name
        #' @return `TRUE` if `Container` has the `name` otherwise `FALSE`
        has_name = function(name) {
            if (missing(name))
                return(any(nzchar(names(self$values()))))

            if (!is.character(name))
                stop("name must be a character string, but got '",
                     data.class(name), "'", call. = FALSE)

            if (length(name) != 1)
                stop("name must be of length 1", call. = FALSE)

            if (is.na(name))
                stop("undefined name", call. = FALSE)

            if (isTRUE(nchar(name) == 0))
                stop("name must consist of at least one character", call. = F)

            isTRUE(utils::hasName(self$values(), name))
        },

        #' @description Check if `Container` is empty
        #' @return `TRUE` if the `Container` is empty else `FALSE`.
        is_empty = function() self$length() == 0,

        #' @description Number of elements of the `Container`.
        #' @return `integer` length of the `Container`, that is, the number of
        #' elements it contains.
        length = function() length(private$elems),

        #' @description Names of the elements.
        #' @return `character` the names of the elements contained in `x`
        names = function() names(private$elems),

        #' @description Same as `peek_at2` (see below) but accepts a vector of
        #' indices and always returns a `Container` object.
        #' @param index vector of indices.
        #' @param default the default value to return in case the value at
        #' `index` is not found.
        #' @return `Container` object with the extracted elements.
        peek_at = function(index, default = NULL) {
            if (missing(index))
                return(self)

            try_at = function(index)
                as.list(tryCatch(self$at(index),
                                 error = function(e) list(default)))

            l = lapply(index, try_at)
            if (identical(l, list()))
                return(methods::as(l, data.class(self)))

            # Determine positions where names need to be set
            isChar = as.logical(sapply(index, is.character))
            hasLen = as.logical(sapply(l, function(x) length(x) > 0))
            pos = which(isChar & hasLen)

            ul = unlist(l, recursive = FALSE)
            names(ul)[pos] <- as.character(index[pos])
            ul = Filter(ul, f = Negate(is.null))

            methods::as(ul, data.class(self))
        },

        #' @description Peek at index and extract value. If index is invalid,
        #' missing, or not not found, return `default` value.
        #' @param index `numeric` or `character` index to be accessed.
        #' @param default the default value to return in case the value at
        #' `index` is not found.
        #' @return the value at the given index or (if not found) the given
        #' default value.
        peek_at2 = function(index, default = NULL) {
            if (missing(index) || !length(index) || self$is_empty())
                return(default)

            .assert_index_arg(index)

            tryCatch(self$at2(index), error = function(e) default)
        },

        #' @description Get value at index and remove it from `Container`.
        #' If `index` is not found, raise an error.
        #' @param index Must be a single number > 0 or a string.
        #' @return If given as a number, the element at the corresponding
        #' position, and if given as a string, the element at the
        #' corresponding name matching the given string is returned.
        pop = function(index) {
            if (self$is_empty())
                stop("pop at empty ", data.class(self), call. = FALSE)

            if (missing(index))
                return(self$pop(self$length()))

            value <- self$at2(index)
            self$delete_at(index)
            value
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

        #' @description Rename a `key` in the `Container`. An error is signaled,
        #' if either the `old` key is not in the `Container` or the `new` key results
        #' in a name-clash with an existing key.
        #' @param old `character` name of key to be renamed.
        #' @param new `character` new key name.
        #' @return the `Container` object
        rename = function(old, new) {
            .rename_check_names(self, old, new)

            if (length(old) > 1) {
                mapply(self$rename, old, new)
                return(self)
            }

            if (identical(old, new))
                return(self)

            if (new %in% names(self))
                stop("name '", new, "' already in ", data.class(self))

            private$.rename(old, new)
            self
        },

        #' @description Replace one element by another element.
        #' Search for occurence of `old` and, if found, replace it by `new`.
        #' If `old` does not exist, an error is signaled, unless `add` was
        #' set to `TRUE`, in which case `new` is added.
        #' @param old element to be replaced
        #' @param new element to be put instead of old
        #' @param add `logical` if `TRUE` the `new` element is added in case
        #' `old` does not exists.
        #' @return the `Container` object
        replace = function(old, new, add = FALSE) {

            pos = private$.get_element_position(old)
            force(new)

            hasElem = !is.na(pos)
            if (!hasElem && add)
                return(self$add(new))

            if (!hasElem)
                stop("old element (", .get_label(old),
                     ") is not in ", data.class(self), call. = FALSE)

            name = names(self)[[pos]]
            private$.replace_value_at(pos, new, name)
            self
        },

        #' @description Replace value at given index.
        #' Replace value at index by given value. If index is not found, an
        #' error is signalled, unless `add` was set to `TRUE`, in which case
        #' `new` is added.
        #' @param index `character` or `numeric` index
        #' @param value `ANY` new value to replace the old one.
        #' @param add `logical` if `TRUE` the new `value` element would be added
        #' in case `index` did not exists.
        #' @return the `Container` object
        replace_at = function(index, value, add = FALSE) {

            hasIndex = .assert_and_has_index(self, index)
            if (!hasIndex && add) {
                name = NULL
                if (is.character(index) && !is.na(index) && nzchar(index))
                    name = index

                return(self$add(value = value, name = name))
            }

            if (!hasIndex)
                .assert_index_and_arg(self, index)

            pos = private$.get_index_position(index)
            name = names(self)[[pos]]

            private$.replace_value_at(pos, value, name)

            self
        },

        #' @description This function is deprecated. Use [delete()] instead.
        #' @param elem element to be deleted from the `Container`. If element
        #'  is not found in the `Container`, an error is signaled.
        #' @return the `Container` object
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
            msg <- paste0("'", old, "()' is deprecated and not useful anymore")
            .Deprecated("mode", msg = msg)
            mode(private$elems)
        },

        #' @description Add elements of `other` to this if the name is
        #' not in the `Container` and update elements with existing names.
        #' @param other `Iterable` object used to update this.
        #' @return returns the `Container`
        update = function(other) {
            if (!is.iterable(other))
                stop("arg must be iterable", call. = FALSE)

            it = other$iter()

            while (it$has_next()) {
                elem = it$get_next()
                self$replace_at(index = names(elem),
                                value = elem[[1]],
                                add = TRUE)
            }

            self
        },

        #' @description Get `Container` values
        #' @return elements of the container as a base list
        values = function() {
            l = private$elems
            if (!any(nzchar(names(l))))
                names(l) <- NULL
            l
        }
    ),
    private = list(
        compare_fun = NULL,
        elems = list(),

        create_iter = function() {
            Iterator$new(self, private$.subset)
        },

        compare_predicate = function(x) {
            function(y) {
                if (is.function(x) || is.function(y))
                    identical(x, y)
                else
                    all(sapply(private$compare_fun(x, y), isTRUE))
            }
        },

        deep_clone = function(name, value) {
            if (name != "elems")
                return(value)

            clone_deep_if_container = function(x) {
                if (is.container(x))
                    x$clone(deep = TRUE) else x
            }
            lapply(value, clone_deep_if_container)
        },

        .get_element_position = function(x, ...) {
            Position(f = private$compare_predicate(x),
                     x = self$values(),
                     ...)
        },

        .get_index_position = function(index) {
            .assert_index_arg(index)
            if (is.numeric(index))
                return(index)

            match(index, names(self), nomatch = 0)
        },

        .rename = function(old, new) {
            pos = match(old, names(self))
            names(private$elems)[pos] = new
        },

        .replace_value_at = function(pos, value, name) {

            if (length(value))
                private$elems[[pos]] = value
            else
                private$elems[pos] = list(value)
        },

        .set_compare_fun = function(x) {
            f = if (is.character(x)) match.fun(x) else x
            private$compare_fun = f
        },

        .verify_same_class = function(x) {
            if (!inherits(x, data.class(self))) {
                stop("arg must be a ", data.class(self), call. = FALSE)
            }
        },

        .subset = function(x, ...) .subset(x$values(), ...),
        .subset2 = function(x, ...) .subset2(x$values(), ...)
    ),
    lock_class = TRUE
)

