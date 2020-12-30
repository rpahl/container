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
#' `data.table`, that is, a dictionary where each element has the same length.
#' Since a dict.table behaves like both a dict and a data.table, all dict and
#' data.table functions and operators can be used as usual.
#'
#' As with [Dict()] objects, it provides reference semantics so that changes
#' like insertion and deletion of elements are done on the original object.
#' @param ... initial elements of the form `key = value` to be put into the
#' `dict.table` and/or additional arguments to be passed to the
#' [data.table::data.table()] constructor. Note that in contrast to
#' [data.table()], [dict.table()] does not allow duplicated keys and therefore
#' always is initialized as `data.table(..., check.names = TRUE)`.
#' @param x any `R` object
#' @name dict.tableS3
#' @import data.table
#' @seealso [dict()], [data.table::data.table()]
#' @export
#' @examples
#' dit = dict.table(x = rep(c("b","a","c"), each = 3), y = c(1,3,6), key = "y")
#' dit
#' setkey(dit, "x")                     # sorts by 'x'
#' dit
#' (add(dit, "v", 1:9))
#' dit[y > 5]
#' (discard(dit, "x"))
#' \dontrun{
#'     getval(dit, "x")                 # column 'x' does not exist
#'     setval(dit, "x", 0)              # cannot be set, if not exist
#' }
#' (setval(dit, "x", 0, add = TRUE))    # ok
#' peek(dit, "x")                       # glance at column
#' has(dit, "x")                        # TRUE
#' pop(dit, "x")                        # get column and remove it
#' has(dit, "x")                        # FALSE
dict.table <- function(...)
{
    dat <- data.table::data.table(..., check.names = TRUE)
    .set_class(dat)
    if (any(duplicated(names(dat)))) {
        dups <- names(dat)[duplicated(names(dat))]
        stop("duplicated keys after init: ", toString(dups))
    }
    dat
}

#' @rdname dict.tableS3
#' @export
as.dict.table <- function(x, ...)
{
    if (is.null(x)) return(dict.table())
    UseMethod("as.dict.table")
}

#' @rdname dict.tableS3
#' @param copy return a copy of the `data.table` object (default) or work on
#' the passed object by reference?
#' @export
#' @examples
#'
#' # Copy and reference semantics for coerced data.table
#' dat = data.table(a = 1)
#' dit = as.dict.table(dat)
#' setval(dit, "a", 9)
#' dit[["a"]]                                   # 9
#' dat[["a"]]                                   # 1
#' dit.dat = as.dict.table(dat, copy = FALSE)   # init by reference
#' setval(dit.dat, "a", 9)
#' dit.dat[["a"]]                               # 9
#' dat[["a"]]                                   # 9
#'
#' # Coerce from dict
#' d = dict(a = 1, b = 1:3)
#' as.dict.table(d)
#'
#' # Coerce from and to data.table
#' dat = data.table(a = 1, b = 2)
#' dit = as.dict.table(dat)
#' is.dict.table(dit)                           # TRUE
#' dat = as.data.table(dit)
#' is.dict.table(dat)                           # FALSE
#' is.data.table(dat)                           # TRUE
as.dict.table.data.table <- function(x, copy = TRUE, ...)
{
    if (copy) {
        dict.table(x)
    } else {
        #.remove_class(x)
        .set_class(x)
    }
}


#' @export
as.dict.table.default <- function(x, ...)
{
    dots = list(...)
    do.call(dict.table, args = c(as.list(x), dots))
}

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
    delete(x, names(x))
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
           "character" = key %in% names(x),
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
    key <- sample(names(x), 1)
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
    data.table::setcolorder(x, sort(names(x), decreasing = decr))
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


