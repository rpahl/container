#' Dict S3 interface
#'
#' @description The [Dict.frame()] is a mix of a dictionary and a
#' `data.frame`. In particular, it is a dictionary thereby inheriting all
#' [Dict()] methods, but where each element has the same length. In contrast to
#' a [base::data.frame()], a [Dict.frame()] can contain arbitrary complex
#' objects.
#' @details For a detailed documentation of all methods see [Dict.frame()].
#' @param x a named vector (or list) of 'any' type including
#' [base::data.frame()]s.
#' @name dict.frameS3
#' @seealso [Dict()], [base::data.frame()]

#' @rdname dict.frameS3
#' @export
dict.frame <- function(...) {
    if (nargs() == 0) return(Dict.frame$new(x = list()))
    x = if (nargs() > 1) list(...) else as.list(...)
    Dict.frame$new(x)
}

#' @name dict.frameS3
#' @export
as.dict.frame <- function(x) dict.frame(x)

#' @name dict.frameS3
#' @export
is.dict.frame <- function(x) inherits(x, "Dict.frame")


#' Extract or replace parts of a `Dict.frame`
#'
#' @description Access and assignment operators for [Dict.frame()] objects.
#' @name dict.frameS3replace
#'
#' @param x [Dict.frame()] object.
#' @param i `integer` row indices of elements to extract or replace.
#' @param j `integer` or `character` column indices of elements to extract
#' or replace.
NULL

#' @rdname dict.frameS3replace
#'
#' @param default A suitable default value.
#' @return For `[[` returns the value at row `i` and column `j` or `default` if
#' the column does not exist. If `i` is not specified (or only one index
#' given), the full column is returned, if it exists or `default` if not.
#' @export
`[[.Dict.frame` <- function(x, i, j, default = NULL)
{
    if (length(i) != 1) stop("i must be of length 1")

    hasDefault <- !missing(default)
    if (hasDefault && !is.null(default)) {
        if (length(default) > nrow(x)) {
            stop("default value cannot exceed number of rows")
        }
        rest = nrow(x) %% length(default)
        if (rest != 0) {
            stop("number of default values must be a multiple of ",
                 "the number of rows (", nrow(x), ")")
        }
        default = rep(default, times = nrow(x) %/% length(default))
    }
    hasComma <- nargs() - hasDefault == 3

    if (hasComma) {
        # x[[i, j]]
        if (length(j) != 1) stop("j must be of length 1")
        xj = if (hasDefault) x[[j, default = default]] else x = x[[j]]
        tryCatch(xj[[i]],
                 error = function(e) stop("row index out of bounds: ", i))
    } else {
        # x[[i]]
        if (is.numeric(i)) {
            i = as.integer(i)
            if (i > x$size()) stop("column index out of bounds: ", i)
            i = x$keys()[i]
        }
        x$peek(i, default = default)
    }
}


#' @rdname dict.frameS3replace
#'
#' @return For `[` returns a `Dict.frame` containing the extracted values.
#' @export
`[.Dict.frame` <- function(x, i, j, default = NULL)
{
    has.i = !missing(i)
    has.j = !missing(j)
    if (missing(i) && missing(j)) return(x)
    hasDefault <- !missing(default)
    if (hasDefault && !is.null(default)) {
        if (length(default) > nrow(x)) {
            stop("default value cannot exceed number of rows")
        }
        rest = nrow(x) %% length(default)
        if (rest != 0) {
            stop("number of default values must be a multiple of ",
                 "the number of rows (", nrow(x), ")")
        }
        default = rep(default, times = nrow(x) %/% length(default))
    }
    hasComma <- nargs() - hasDefault == 3

    d = dict.frame()
    if (hasComma) {
        dj = if (hasDefault) x[j, default = default] else x[j]
        if (has.i && nrow(dj) > 0) {
            # x[i, j] or x[i, ]
            if (any(i > nrow(x))) {
                iBad = setdiff(i, seq_len(nrow(x)))
                stop("row indices out of bounds: " , toString(iBad))
            }
            for (key in keys(dj)) {
                d$add(key, dj[[key]][i])
            }
            d$set_rownames(as.integer(i))
        } else {
            # x[, j]
            d = dj
        }
    } else {
        # x[i]
        for (key in unique(i)) {
            if (is.numeric(key)) {
                key = as.integer(key)
                if (key > x$size()) stop("column index out of bounds: ", key)
                key = keys(x)[key]
            }
            if (!x$has(key) && !hasDefault) {
                stop("column '", key, "' not found")
            }
            value = x$peek(key, default = default)
            d$add(key, value)
        }
    }
    d
}


#' @rdname dict.frameS3replace
#'
#' @param add `logical` If `FALSE` and column does not exist, an error is
#' signaled. This is different from [base::data.frame()]s, where instead a new
#' column would be generated. To behave like standard [base::data.frame()]s in
#' this scenario, set `add = TRUE`, which replaces the value at position i, j,
#' but will also add a new column if the column is not yet in the
#' [dict.frame()]. To be consistent with the access operators, this 'add'
#' operation is only defined if `j` is a `character` index.
#' Similarly and also in contrast to [base::data.frame()]s, a new row may never
#' by substituted, that is, if the row index exceeds the number of rows, the
#' function will always give an error.
#' @param value A suitable replacement value.
#' @return For `[[<-` replaces the value at position `[i, j]`. If a new column
#' is generated, the value is replicated in each row.
`[[<-.Dict.frame` <- function(x, i, j, add = FALSE, value)
{
    #browser()
    if (length(i) != 1) stop("i must be of length 1")
    hasAdd <- !missing(add)
    hasComma <- nargs() - hasAdd == 4

    if (hasComma) {
        # x[[i, j]] <- value
        if (length(j) != 1) stop("j must be of length 1")

        current = x[[i, j]]


        x[[j, add = add]][[i]] <- value
    } else {
        # x[[i]] <- value
        if (nrow(x) > 1) {
            if (!is.atomic(value)) {
                stop("if only 'i' is defined, value must be atomic ",
                     "unless dict.fram consists of just one row")
            }
            value =  rep(value, nrow(x))
        }
        x$set(i, value, add = TRUE)
    }


    if (is.numeric(i)) {
        i = as.integer(i)
        if (i > x$size()) stop(i, ": subscript out of bounds")
        i = x$keys()[i]
    }
    if (!x$has(i) && !add) stop("column '", i, "' not found")


}


# Conversion and functions related to data.frame

#' @export
`as.data.frame.Dict.frame` <- function(x, ...)
{
    is_convertible = all(sapply(x$values(), is.atomic))
    if (!is_convertible) {
        stop("Dict.frame must consist of atomic columns to be convertible to data.frame")
    }
    df = as.data.frame(x$values(), ...)
    attr(df, "row.names") <- rownames(x)
    df
}

#' @export
`dimnames.Dict.frame` <- function(x)
{
    # This functions enables calling rownames and colnames on Dict.frame objects.
    list(x$rownames(), x$keys())
}

#' @export
`dim.Dict.frame` <- function(x)
{
    # This functions enables calling nrow and ncol on Dict.frame objects.
    c(length(x$rownames()), length(x$keys()))
}


# TODO: rowbind.Dict.frame
