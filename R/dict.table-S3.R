.remove_class <- function(x, class = "dict.table")
{
    stopifnot(length(class) == 1L)

    class.new <- attr(x, "class")
    pos <- match(class.new, class)
    if (!is.na(pos)) {
        class.new <- class.new[-pos]
    }
    data.table::setattr(x, "class", class.new)
}

.set_class <- function(x, class = "dict.table")
{
    class.new <- unique(c("dict.table", attr(x, "class")))
    data.table::setattr(x, "class", class.new)
}

#' dict.table
#'
#' @description The [dict.table()] is a mix of a dictionary and a
#' `data.table`. In particular, it is a dictionary thereby inheriting all
#' [Dict()] methods, but where each element has the same length. In contrast to
#' a [base::data.frame()], a [dict.table()] can contain arbitrary complex
#' objects.
#' @details For a detailed documentation of all methods see [Dict.table()].
#' @param x a `dict.table` object.
#' @param ... arguments of the form tag = value or a named list
#' @name dict.tableS3
#' @import data.table
#' @seealso [Dict()], [data.table:data.table()], [`[[<-.dict.table()`],
#' [`[[.dict.table()`], [`[<-.dict.table()`], [`[.dict.table()`]
NULL

#' @rdname dict.tableS3
#' @export
dict.table <- function(...) {

    dat <- data.table::data.table(..., check.names = TRUE)
    .set_class(dat)
}

#' @rdname dict.tableS3
#' @export
as.dict.table <- function(x, ...) UseMethod("as.dict.table")

#' @rdname dict.tableS3
#' @export
as.dict.table.data.table <- function(x, copy = TRUE, ...)
{
    if (copy) {
        dict.table(x)
    } else {
        .remove_class(x)
    }
}

#' @rdname dict.tableS3
#' @export
as.dict.table.default <- function(x) dict.table(x)

#' @rdname dict.tableS3
#' @export
is.dict.table <- function(x) inherits(x, "dict.table")


# Common Container methods

#' @rdname dict.tableS3
#' @export
add.dict.table <- function(x, key, value)
{
    if (has(x, key)) {
        stop("key '", key, "' already in ", data.class(x))
    }
    setval(x, key, value, add = TRUE)
}

#' @rdname dict.tableS3
#' @export
clear.dict.table <- function(x)
{
    delete(x, keys(x))
}


#' @rdname dict.tableS3
#' @export
delete.dict.table <- function(x, key)
{
    has_key <- function(key) has(x, key)
    missing_cols = Filter(key, f = Negate(has_key))
    if (length(missing_cols)) {
        col_str = paste0("'", missing_cols, "'", collapse = ", ")
        stop("Column", ifelse(length(missing_cols) > 1, "s ", " "),
             col_str,
             ifelse(is.character(key),
                    paste(" not in", data.class(x)),
                    paste0(" out of range (ncol = ", ncol(x), ")")))
    }
    discard(x, key)
}


#' @rdname dict.tableS3
#' @export
discard.dict.table <- function(x, key)
{
    j = Filter(unique(key), f = function(key) has(x, key))
    if (is.numeric(j)) j = as.integer(j)

    if (length(j)) {
        data.table::set(x, j = j, value = NULL)
    }
    invisible(x)
}


#' @rdname dict.tableS3
#' @export
empty.dict.table <- function(x)
{
    ncol(x) == 0
}


#' @rdname dict.tableS3
#' @export
getval.dict.table <- function(x, key)
{
    if (has(x, key)) {
        peek(x, key)
    } else {
        stop("key '", key, "' not in ", data.class(x))
    }
}


#' @rdname dict.tableS3
#' @export
has.dict.table <- function(x, key)
{
    if (length(key) != 1) stop("key must be of length 1")
    if (is.na(key)) stop("undefined key")
    switch(data.class(key),
           "character" = key %in% keys(x),
           "numeric" = ncol(x) >= key,
           stop("key must be character or numeric")
    )
}

#' @rdname dict.tableS3
#' @export
keys.dict.table <- function(x)
{
    colnames(x)
}


#' @rdname dict.tableS3
#' @export
peek.dict.table <- function(x, key, default = NULL)
{
    if (has(x, key)) {
        as.list(x)[[key]]
    } else {
        rep(default, times = nrow(x) %/% length(default))
    }
}


#' @rdname dict.tableS3
#' @export
pop.dict.table <- function(x, key)
{
    elem <- peek(x, key)
    delete(x, key)
    elem
}


#' @rdname dict.tableS3
#' @export
popitem.dict.table <- function(x)
{
    if (empty(x)) {
        stop("pop at empty ", data.class(x))
    }
    key <- sample(keys(x), 1)
    pop(x, key)
}


#' @rdname dict.tableS3
#' @export
print.dict.table <- function(x, ...)
{
    cat0 <- function(...) cat(..., sep = "")
    class_name <- paste0("<", data.class(x), ">")
    cat0(class_name, " with ",
         nrow(x), " row", ifelse(nrow(x) == 1, "", "s"), " and ",
         ncol(x), " column", ifelse(ncol(x) == 1, "", "s"), "\n")
    print(as.data.table(x), ...)
}


#' @rdname dict.tableS3
#' @export
rbind.dict.table <- function(x, ...)
{
    res <- do.call(rbind, args = c(list(as.data.table(x)), list(...)))
    .set_class(res)
}


#' @rdname dict.tableS3
#' @export
rename.dict.table <- function(x, old, new)
{
    data.table::setnames(x, old, new)
}


#' @rdname dict.tableS3
#' @export
setval.dict.table <- function(x, key, value, add = FALSE)
{
    if (!add) {
        if (!has(x, key)) {
            stop(ifelse(is.numeric(key), "column", "key"),
                 " '", key, "' not in ", data.class(x))
        }
    }
    j <- if (is.numeric(key)) as.integer(key) else key
    data.table::set(x, j = j, value = value)
}


#' @rdname dict.tableS3
#' @export
size.dict.table <- function(x)
{
    ncol(x)
}


#' @rdname dict.tableS3
#' @export
sortkey.dict.table <- function(x, decr = FALSE)
{
    data.table::setcolorder(x, sort(keys(x), decreasing = decr))
}


#' @rdname dict.tableS3
#' @export
values.dict.table <- function(x)
{
    as.data.table(x)
}



#' Extract or replace parts of a `dict.table`
#'
#' @description Access and assignment operators for [dict.table()] objects.
#' @name dict.table.replace
#'
#' @param x [dict.table()] object.
#' @param i `integer` row indices of elements to extract or replace.
#' @param j `integer` or `character` column indices of elements to extract
#' or replace.
NULL


# Coercion to other classes

#' @export
`as.data.frame.dict.table` <- function(x, ...)
{
    as.data.frame(as.list(x), ...)
}

#' @export
`as.data.table.dict.table` <- function(x, copy = TRUE, ...)
{
    if (copy) {
        data.table::as.data.table(as.list(x), ...)
    } else {
        data.table::setattr(x, "class", attr(x, "class")[-1])
    }
}


