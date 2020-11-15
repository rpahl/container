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
#' @return For `[[` returns the value at row `i` and column `j`. If the column
#' is specified as an integer and does not exist, an out of bounds error is
#' raised. If specified as character, `NULL` is returned unless `default` was
#' specified, in which case the `default` value is returned.
#' If `i` is not specified (or only one index given), the full column is
#' returned, again, following the above rules in case of missing column.
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
#' @return For `[` returns the values at rows `i` and columns `j`. For any of
#' the columns being specified as an integer that do not exist, an out of
#' bounds error is raised. If specified as character, an error is raised
#' unless `default` was specified, in which case the `default` value is
#' returned. In this case, the `default` values must be a multiple of the
#' number of rows. Note that it is possible to use a mixed
#' `integer`/`character` specification of columns passed as a list.
#' If `i` is not specified (or only one index given), the full columns are
#' returned, again, following the above rules in case of missing columns.
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
#' @param add `logical` If only one column index was used (i.e. x[[i]], and
#' `add` set to `TRUE` and the column index is of type `character` then the
#' the value(s) are added as a new column to the `dict.frame`. In all other
#' cases or if `add == FALSE`, an out of bounds error is signaled.
#' @param value A suitable replacement value.
#' @return For `[[<-` replaces the value at position `[i, j]`. If a new column
#' is generated (see option `add`), the value is replicated if needed. Note
#' that for this the value has to be a multiple of the number of rows. In
#' contrast to [base::data.frame()]s, numerical row or column indices that
#' exceed the dimensions of the [dict.frame()] will always stop with an error,
#' that is, new rows or columns are never substituted. The only way to attach
#' a new column is to use a single character index and explicitly stating the
#' intention to add the column, i.e., `x[["A", add = TRUE]] <- 1`.
#' @export
`[[<-.Dict.frame` <- function(x, i, j, add = FALSE, value)
{
    if (length(i) != 1) stop("i must be of length 1")
    hasAdd <- !missing(add)
    hasComma <- nargs() - hasAdd == 4

    if (hasComma) {
        # x[[i, j]] <- value
        if (length(value) != 1) stop("value must be of length 1")
        if (length(j) != 1) stop("j must be of length 1")

        # Try to access element and get key
        current = x[i, j]
        key = if (is.numeric(j)) x$keys()[as.integer(j)] else j

        xj = x[[key]]
        xj[[i]] <- value
        x$set(key, xj)
    } else {
        # x[[i]] <- value

        # Try to access element and get key
        current = if (add) x[[i]] else x[i]
        key = if (is.numeric(i)) x$keys()[as.integer(i)] else i
        if (is.null(value)) return(x$discard(key))

        if (nrow(x) > 0) {
            rest = nrow(x) %% length(value)
            if (rest != 0) {
                stop("number of values must be a multiple of ",
                     "the number of rows (", nrow(x), ")")
            }
            value = rep(value, times = nrow(x) %/% length(value))
        }
        x$set(key, value, add = add)
    }
    invisible(x)
}


#' @rdname dict.frameS3replace
#'
#' @param add `logical` If only the column index was set (i.e. x[i] or x[, j]),
#' and `add` set to `TRUE` and the column index is of type `character` then the
#' the value(s) are added as a new column to the `dict.frame`. In all other
#' cases or if `add == FALSE`, an out of bounds error is signaled.
#' @param value A suitable replacement value.
#' @return For `[<-` replaces the value(s) at positions `[i, j]`. In
#' contrast to [base::data.frame()]s, numerical row or column indices that
#' exceed the dimensions of the [dict.frame()] will always stop with an error,
#' that is, new rows or columns are never substituted. The only way to attach
#' new columns is to use character indices, i.e., `x["A", add = TRUE] <- 1` or
#' `x[, c("A", "C"), add = TRUE] <- 9` .
#' @export
`[<-.Dict.frame` <- function(x, i, j, add = FALSE, value)
{
    has.i = !missing(i)
    has.j = !missing(j)
    if (missing(i) && missing(j)) return(x)
    hasAdd <- !missing(add)
    hasComma <- nargs() - hasAdd == 4

    if (hasComma) {
        if (!has.i || identical(i, seq_len(nrow(x)))) {
            # x[, j]
            if (hasAdd) x[j, add = add] <- value else x[j] <- value
        } else {
            if (!has.j) {
                j = seq_len(ncol(x))
                if (hasAdd) x[i, j, add = add] <- value else x[i, j] <- value
                return(invisible(x))
            }

            # x[i, j] <- value
            if (length(value) != length(i) && length(value) != 1) {
                stop("length of value must either match number of rows (",
                     length(i), ") or 1")
            }
            # Set values one by one
            for (ii in unique(i)) {
                val = if (length(value) > 1) value[[ii]] else value
                for (jj in unique(j)) {
                    if (hasAdd) {
                        x[[ii, jj, add = add]] <- val
                    } else {
                        x[[ii, jj]] <- val
                    }
                }
            }
        }
    } else {
        # x[i] <- value
        for (ii in unique(i)) {
            if (hasAdd) {
                x[[ii, add = add]] <- value
            } else {
                x[[ii]] <- value
            }
        }
    }
    invisible(x)
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
