#' Add elements
#'
#' Add elements to container-like objects.
#' @param x an `R` object of the respective class.
#' @param ... elements to be added. If `x` is a `Dict` or `dict.table` object,
#' all elements must be named, that is, of the form `key = value`.
#' @export
#' @examples
#' co = container(1)
#' add(co, 1, b = 2, c = container(1:3))
#'
#' s = setnew(1)
#' add(s, 1, 1, b = 2, "1", co = container(1, 1))
#'
add <- function(x, ...) UseMethod("add")

#' @rdname add
#' @return For `Container`, an object of class `Container` (or of the
#' respective derived class) with the elements being added to `x`.
#' @export
add.Container <- function(x, ...)
{
    x$clone(deep = TRUE)$add(...)
}

#' @name add.Container
#' @rdname ContainerS3
#' @usage ## S3 method for class 'Container'
#' add(x, ...)
#' @details * `add(x, ...)` add elements to `x`.
#' @examples
#' co = container()
#' add(co, 1, b = 2, c = container(1:3))
NULL


#' @rdname add
#' @return For `Dict`, an object of class `Dict` with the key-value pairs being
#' added to `x`. If one of the keys already exists, an error is given.
#' @export
#' @examples
#' d = dict(a = 1)
#' add(d, b = 2, co = container(1:3))
#'
#' \dontrun{
#' add(d, a = 7:9)  # key 'a' already in Dict}
#'
add.Dict <- function(x, ...)
{
    d = as.dict(x) # create copy

    elems = list(...)

    if (length(elems) == 0)
        return(d)

    elem_names = names(elems)

    if (!length(elem_names) ||
        !all(sapply(elem_names, is_nonempty_string)))
        stop("all elements must be named")

    for (i in seq_along(elems))
        d$add(elem_names[[i]], elems[[i]])

    d
}

#' @name add.Dict
#' @rdname DictS3
#' @usage ## S3 method for class 'Dict'
#' add(x, ...)
#' @details * `add(x, ...)` adds `key = value` pairs to `x`. If one of the
#' keys already exists, an error is given.
NULL



#' @rdname add
#' @return For `dict.table`, an object of class `dict.table` with the columns
#' being added to `x`. All given columns must be named. If one of the column
#' names already exists, an error is given.
#' @export
#' @examples
#' dit = dict.table(a = 1:3)
#' add(dit, b = 3:1, d = 4:6)
#'
#' \dontrun{
#' add(dit, a = 7:9)  # column 'a' already exists}
#'
add.dict.table <- function(x, ...)
{
    elems = list(...)

    if (length(elems) == 0)
        return(x)

    elem_names = names(elems)

    if (!length(elem_names) ||
        !all(sapply(elem_names, is_nonempty_string)))
        stop("all elements must be named")

    common_names = intersect(colnames(x), elem_names)
    hasNameCollision = length(common_names) > 0
    if (hasNameCollision)
        stop("column(s) ", toString(paste0("'", common_names, "'")),
             " exist(s) already")

    for (i in seq_along(elems))
        replace(x, elem_names[[i]], elems[[i]], add = TRUE)

    x
}


#' @name add.dict.table
#' @rdname dict.table
#' @usage ## S3 method for class 'dict.table'
#' add(x, ...)
#' @details * `add(x, ...)` adds `key = value` pairs to `x`. If one of the
#' keys already exists, an error is given.
#' @examples
#' dit = dict.table(a = 1:3)
#' add(dit, b = 3:1, d = 4:6)
#'
#' \dontrun{
#' add(dit, a = 7:9)  # column 'a' already exists}
#'
NULL


#' @rdname add
#' @export
addleft <- function(x, ...) UseMethod("addleft")

#' @rdname add
#' @return For `Deque`, an object of class `Deque` with the elements being
#' added to the right or left (`addleft`) of `x`.
#' @export
#' @examples
#' d = deque(0)
#' add(d, a = 1, b = 2)         # |0, a = 1, b = 2|
#' addleft(d, a = 1, b = 2)     # |b = 2, a = 1, 0|
addleft.Deque <- function(x, ...)
{
    x$clone(deep = TRUE)$addleft(...)
}


#' @name addleft.Deque
#' @rdname DequeS3
#' @usage ## S3 method for class 'Deque'
#' addleft(x, ...)
#' @details * `addleft(x, ...)` add (possibly named) elements to left
#' side of `x`.
#' @examples
#' d = deque(0)
#' add(d, a = 1, b = 2)         # |0, a = 1, b = 2|
#' addleft(d, a = 1, b = 2)     # |b = 2, a = 1, 0|
NULL

