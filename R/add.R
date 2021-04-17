#' Add elements
#'
#' Add elements to container-like objects.
#' @param x an `R` object of the respective class.
#' @param ... elements to be added.
#' @details Both `add` and `addleft` use copy semantics while `add_` and
#' `addleft_` work by reference.
#' @export
add <- function(x, ...) UseMethod("add")

#' @rdname add
#' @export
add_ <- function(x, ...) UseMethod("add_")

#' @rdname add
#' @export
addleft <- function(x, ...) UseMethod("addleft")

#' @rdname add
#' @export
addleft_ <- function(x, ...) UseMethod("addleft_")


#' @rdname add
#' @return For `Container`, an object of class `Container` (or one of the
#' respective derived classes).
#' @details
#' If `x` is a `Container`, `Set` or `Deque` object, the elements being added
#' can (but must not) be named.
#' @export
#' @examples
#'
#' co = container(1)
#' add(co, 1, b = 2, c = container(1:3))
#'
#' s = setnew(1)
#' add(s, 1, 1, b = 2, "1", co = container(1, 1))
add.Container <- function(x, ...)
{
    (add_(x$clone(deep = TRUE), ...))
}

#' @name add.Container
#' @rdname ContainerS3
#' @usage
#' add(x, ...)
#' add_(x, ...)
#' @details
#' * `add(x, ...)` and `add_(x, ...)` add elements to `x`.
#' @examples
#'
#' co = container(1)
#' add(co, 1, b = 2, c = container(1:3))
#'
#' s = setnew(1)
#' add(s, 1, 1, b = 2, "1", co = container(1, 1))
NULL

#' @rdname add
#' @export
add_.Container <- function(x, ...)
{
    invisible(x$add(...))
}


#' @rdname add
#' @return For `Deque`, an object of class `Deque` with the elements being
#' added to the right or left (`addleft`) of `x`.
#' @export
#' @examples
#'
#' d = deque(0)
#' add(d, a = 1, b = 2)         # |0, a = 1, b = 2|
#' addleft(d, a = 1, b = 2)     # |b = 2, a = 1, 0|
addleft.Deque <- function(x, ...)
{
    (addleft_(x$clone(deep = TRUE), ...))
}

#' @name addleft.Deque
#' @rdname DequeS3
#' @usage
#' addleft(x, ...)
#' addleft_(x, ...)
#' @details
#' * `addleft(x, ...)` adds (possibly named) elements to left side of `x`.
#' * `addleft_(x, ...)` same as `addleft(x, ...)` but adds by reference.
#' @examples
#'
#' d = deque(0)
#' add(d, a = 1, b = 2)         # |0, a = 1, b = 2|
#' addleft(d, a = 1, b = 2)     # |b = 2, a = 1, 0|
NULL

#' @rdname add
#' @export
addleft_.Deque <- function(x, ...)
{
    invisible(x$addleft(...))
}


#' @rdname add
#' @return For `Dict`, an object of class `Dict`.
#' @details
#' If `x` is a `Dict` or `dict.table` object, all elements *must* be of the
#' form `key = value`. If one of the keys already exists, an error is given.
#' @export
#' @examples
#'
#' d = dict(a = 1)
#' add(d, b = 2, co = container(1:3))
#'
#' \dontrun{
#' add(d, a = 7:9)  # key 'a' already in Dict}
add.Dict <- function(x, ...)
{
    (add_(x$clone(deep = TRUE), ...))
}

#' @name add.Dict
#' @rdname DictS3
#' @usage
#' add(x, ...)
#' add_(x, ...)
#' @details
#' * `add(x, ...)` adds `key = value` pairs to `x`. If one of the
#' keys already exists, an error is given.
#' * `add_(x, ...)` same as `add(x, ...)` but adds by reference.
#' @examples
#'
#' d = dict(a = 1)
#' add(d, b = 2, co = container(1:3))
#'
#' \dontrun{
#' add(d, a = 7:9)  # key 'a' already in Dict}
NULL


#' @rdname add
#' @export
add_.Dict <- function(x, ...)
{
    elems = list(...)
    if (length(elems) == 0)
        return(x)

    elem_names = names(elems)

    verify_names(elem_names)
    check_name_collision(names(x), elem_names)

    for (i in seq_along(elems))
        x$add(elem_names[[i]], elems[[i]])

    invisible(x)
}



#' @rdname add
#' @return For `dict.table`, an object of class `dict.table`.
#' @export
#' @examples
#'
#' dit = dict.table(a = 1:3)
#' add(dit, b = 3:1, d = 4:6)
#'
#' \dontrun{
#' add(dit, a = 7:9)  # column 'a' already exists}
add.dict.table <- function(x, ...)
{
    (add_(copy(x), ...))
}


#' @name add.dict.table
#' @rdname dict.table
#' @usage
#' add(x, ...)
#' add_(x, ...)
#' @details
#' * `add(x, ...)` and add_(x, ...) add columns to `x`. If the column name
#' already exists, an error is given.
#' @examples
#'
#' dit = dict.table(a = 1:3)
#' add(dit, b = 3:1, d = 4:6)
#'
#' \dontrun{
#' add(dit, a = 7:9)  # column 'a' already exists}
NULL


#' @rdname add
#' @export
#' @examples
#'
#' dit = dict.table(a = 1:3)
#' add(dit, b = 3:1, d = 4:6)
#'
#' \dontrun{
#' add(dit, a = 7:9)  # column 'a' already exists}
#'
add_.dict.table <- function(x, ...)
{
    elems = list(...)
    if (length(elems) == 0)
        return(x)

    elem_names = names(elems)

    verify_names(elem_names)
    check_name_collision(colnames(x), elem_names)

    for (i in seq_along(elems))
        replace_.dict.table(x, elem_names[[i]], elems[[i]], add = TRUE)

    invisible(x)
}

