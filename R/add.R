#' @name ContainerS3methods
#' @rdname ContainerS3
#' @usage
#' ## More container S3 methods
NULL

#' Add elements
#'
#' Add elements to container-like objects.
#' @param .x an `R` object of the respective class.
#' @param ... elements to be added.
#' @details Both `add` and `addleft` use copy semantics while `ref_add` and
#' `ref_addleft` work by reference.
#' @export
add <- function(.x, ...) UseMethod("add")

#' @rdname add
#' @export
ref_add <- function(.x, ...) UseMethod("ref_add")


#' @name DequeS3methods
#' @rdname DequeS3
#' @usage
#' ## More deque S3 methods
NULL

#' @rdname add
#' @export
addleft <- function(.x, ...) UseMethod("addleft")

#' @rdname add
#' @export
ref_addleft <- function(.x, ...) UseMethod("ref_addleft")


#' @rdname add
#' @return For `Container`, an object of class `Container` (or one of the
#' respective derived classes).
#' @details
#' If `.x` is a `Container`, `Set` or `Deque` object, the elements being added
#' can (but must not) be named.
#' @examples
#'
#' co = container(1)
#' add(co, 1, b = 2, c = container(1:3))
#'
#' s = setnew(1)
#' add(s, 1, 1, b = 2, "1", co = container(1, 1))
#' @export
add.Container <- function(.x, ...)
{
    (ref_add(.x$clone(deep = TRUE), ...))
}

#' @name ContainerS3methods
#' @rdname ContainerS3
#' @usage
#' add(.x, ...)
#' ref_add(.x, ...)
#' @details
#' * `add(.x, ...)` and `ref_add(.x, ...)` add elements to `.x`.
#' @examples
#' co = container(1)
#' add(co, 1, b = 2, c = container(1:3))
NULL

#' @rdname add
#' @export
ref_add.Container <- function(.x, ...)
{
    elems = list(...)
    elem_names = names(elems)

    for (i in seq_along(elems))
        .x$add(elems[[i]], name = elem_names[[i]])

    invisible(.x)
}


#' @rdname add
#' @return For `Deque`, an object of class `Deque` with the elements being
#' added to the right or left (`addleft`) of `.x`.
#' @export
#' @examples
#' d = deque(0)
#' add(d, a = 1, b = 2)         # |0, a = 1, b = 2|
#' addleft(d, a = 1, b = 2)     # |b = 2, a = 1, 0|
addleft.Deque <- function(.x, ...)
{
    (ref_addleft(.x$clone(deep = TRUE), ...))
}

#' @name DequeS3methods
#' @rdname DequeS3
#' @usage
#' addleft(.x, ...)
#' ref_addleft(.x, ...)
#' @details
#' * `addleft(.x, ...)` adds (possibly named) elements to left side of `.x`.
#' * `ref_addleft(.x, ...)` same as `addleft(.x, ...)` but adds by reference.
#' @examples
#'
#' d = deque(0)
#' add(d, a = 1, b = 2)         # |0, a = 1, b = 2|
#' addleft(d, a = 1, b = 2)     # |b = 2, a = 1, 0|
NULL

#' @rdname add
#' @export
ref_addleft.Deque <- function(.x, ...)
{
    elems = list(...)
    elem_names = names(elems)

    for (i in seq_along(elems))
        .x$addleft(elems[[i]], name = elem_names[[i]])

    invisible(.x)
}

#' @rdname add
#' @return For `Dict`, an object of class `Dict`.
#' @details
#' If `.x` is a `Dict` or `dict.table` object, all elements *must* be of the
#' form `key = value`. If one of the keys already exists, an error is given.
#' @export
#' @examples
#'
#' d = dict(a = 1)
#' add(d, b = 2, co = container(1:3))
#'
#' \dontrun{
#' add(d, a = 7:9)  # key 'a' already in Dict}
#'
add.Dict <- function(.x, ...)
{
    (ref_add(.x$clone(deep = TRUE), ...))
}

#' @name DictS3methods
#' @rdname DictS3
#' @usage
#' ## More dict S3 methods
NULL

#' @rdname DictS3
#' @usage
#' add(.x, ...)
#' ref_add(.x, ...)
#' @details
#' * `add(.x, ...)` adds `key = value` pairs to `.x`. If one of the
#' keys already exists, an error is given.
#' * `ref_add(.x, ...)` same as `add(.x, ...)` but adds by reference.
#' @examples
#'
#' d = dict(a = 1)
#' add(d, b = 2, co = container(1:3))
#'
#' \dontrun{
#' add(d, a = 7:9)  # key 'a' already in Dict}
#'
NULL


#' @rdname add
#' @export
ref_add.Dict <- function(.x, ...)
{
    elems = list(...)
    if (!length(elems))
        return(.x)

    elem_names = names(elems)

    verify_names(elem_names)
    check_name_collision(names(.x), elem_names)

    for (i in seq_along(elems))
        .x$add(elem_names[[i]], elems[[i]])

    invisible(.x)
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
add.dict.table <- function(.x, ...)
{
    (ref_add.dict.table(copy(.x), ...))
}


#' @name dict.tableMethods
#' @rdname dict.table
#' @usage
#' add(.x, ...)
#' ref_add(.x, ...)
#' @details
#' * `add(.x, ...)` and `ref_add(.x, ...)` add columns to `.x`. If the column name
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
ref_add.dict.table <- function(.x, ...)
{
    elems = list(...)
    if (length(elems) == 0)
        return(.x)

    elem_names = names(elems)

    verify_names(elem_names)
    check_name_collision(colnames(.x), elem_names)

    for (i in seq_along(elems))
        ref_replace_at.dict.table(.x, elem_names[[i]], elems[[i]], .add = TRUE)

    invisible(.x)
}

