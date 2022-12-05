#' Add Elements to Containers
#'
#' Add elements to container-like objects.
#' @param .x an `R` object of the respective class.
#' @param ... elements to be added.
#' @note While [add] uses copy semantics [ref_add] works by reference.
#' @export
add <- function(.x, ...) UseMethod("add")

#' @rdname add
#' @export
ref_add <- function(.x, ...) UseMethod("ref_add")


#' @rdname add
#' @return For [Container], an object of class [Container] (or one of the
#' respective derived classes).
#' @note
#' If `.x` is a [Container], [Set] or [Deque] object, the elements being added
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

#' @name ContainerS3
#' @rdname ContainerS3
#' @details
#' * `add(.x, ...)` and `ref_add(.x, ...)` add elements to `.x`.
#' @examples
#'
#' co = container(1)
#' add(co, 1, b = 2, c = container(1:3))
#'
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
#' @note
#' If `.x` is a [Dict] or [dict.table] object,
#' all elements *must* be of the form `key = value`.
#' If one of the keys already exists, an error is given.
#' @export
#' @examples
#'
#' d = dict(a = 1)
#' add(d, b = 2, co = container(1:3))
#'
#' try(add(d, a = 7:9))  # key 'a' already in Dict
add.Dict <- function(.x, ...)
{
    (ref_add(.x$clone(deep = TRUE), ...))
}


#' @name DictS3
#' @rdname DictS3
#' @details
#' * `add(.x, ...)` and `ref_add(.x, ...)` adds `key = value` pairs to `.x`.
#' If any of the keys already exists, an error is given.
#' @examples
#'
#' d = dict(a = 1)
#' add(d, b = 2, co = container(1:3))
#'
#' try(add(d, a = 7:9))  # key 'a' already in Dict
NULL


#' @rdname add
#' @export
ref_add.Dict <- function(.x, ...)
{
    elems = list(...)
    if (!length(elems))
        return(.x)

    elem_names = names(elems)

    .verify_names(elem_names)
    .check_name_collision(names(.x), elem_names)

    for (i in seq_along(elems))
        .x$add(elem_names[[i]], elems[[i]])

    invisible(.x)
}



#' @rdname add
#' @return For [dict.table] an object of class
#' [dict.table].
#' @export
#' @examples
#'
#' dit = dict.table(a = 1:3)
#' add(dit, b = 3:1, d = 4:6)
#'
#' try(add(dit, a = 7:9))  # column 'a' already exists
add.dict.table <- function(.x, ...)
{
    (ref_add.dict.table(copy(.x), ...))
}


#' @name dict.table
#' @rdname dict.table
#' @details
#' * `add(.x, ...)` and `ref_add(.x, ...)` add columns to `.x`. If the column name
#' already exists, an error is given.
#' @examples
#'
#' dit = dict.table(a = 1:3)
#' add(dit, b = 3:1, d = 4:6)
#'
#' try(add(dit, a = 7:9))  # column 'a' already exists
NULL


#' @rdname add
#' @export
#' @examples
#'
#' dit = dict.table(a = 1:3)
#' add(dit, b = 3:1, d = 4:6)
#'
#' try(add(dit, a = 7:9))  # column 'a' already exists
ref_add.dict.table <- function(.x, ...)
{
    elems = list(...)
    if (length(elems) == 0)
        return(.x)

    elem_names = names(elems)

    .verify_names(elem_names)
    .check_name_collision(colnames(.x), elem_names)

    for (i in seq_along(elems))
        ref_replace_at.dict.table(.x, elem_names[[i]], elems[[i]], .add = TRUE)

    invisible(.x)
}

