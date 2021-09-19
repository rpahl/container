.remove_class <- function(x, class = "dict.table")
{
    stopifnot(length(class) == 1L)

    class.new <- attr(x, "class")
    pos <- match(class, class.new)
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


#' @title dict.table
#'
#' @description The [dict.table()] is a mix of a [dict()] and a
#' `data.table`, that is, it can be considered as a data.table with extended
#' functionality to manage its data columns.
#' As a result, a dict.table object provides all [dict()] and [data.table()]
#' functions and operators at the same time.
#'
#' @param ... elements put into the `dict.table` and/or additional arguments
#' to be passed on.
#' @param x any `R` object or a `dict.table` object.
#' @name dict.table
#' @note In contrast to [data.table()], [dict.table()] does not allow duplicated keys.
#' @import data.table
#' @seealso [dict()], [data.table::data.table()]
#' @examples
#' # Some basic examples using some typical data.table and dict operations.
#' # The constructor can take the 'key' argument known from data.table():
#' require(data.table)
#' dit = dict.table(x = rep(c("b","a","c"), each = 3), y = c(1,3,6), key = "y")
#' print(dit)
#' setkey(dit, "x")                             # sort by 'x'
#' print(dit)
#' (add(dit, "v" = 1:9))                        # add column v = 1:9
#' dit[y > 5]
#' (ref_discard_at(dit, "x"))                   # discard column 'x'
#'
#' \dontrun{
#' at(dit, "x")                                 # index 'x' not found
#' replace_at(dit, "x" = 0)                     # cannot be replaced, if it does not exist
#' }
#'
#' dit = replace_at(dit, "x" = 0, .add = TRUE)  # ok - re-adds column 'x' with all 0s
#' peek_at(dit, "x")                            # glance at column 'x'
#' has_name(dit, "x")                           # TRUE
#' ref_pop(dit, "x")                            # get column and remove it
#' has_name(dit, "x")                           # FALSE
#'
NULL


#' @rdname dict.table
#' @details
#' * `dict.table(...)` initializes and returns a [dict()] object.
#' @export
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

#' @rdname dict.table
#' @details
#' * `as.dict.table(x, ...)` coerces `x` to a `dict.table`
#' @export
as.dict.table <- function(x, ...)
{
    if (is.null(x)) return(dict.table())
    UseMethod("as.dict.table")
}

#' @rdname dict.table
#' @param copy if `TRUE` creates a copy of the `data.table` object otherwise
#' works on the passed object by reference.
#' @export
#' @examples
#'
#' # Copy and reference semantics when coercing *from* a data.table
#' dat = data.table(a = 1, b = 2)
#' dit = as.dict.table(dat)
#' is.dict.table(dit)                           # TRUE
#' is.dict.table(dat)                           # FALSE
#' ref_replace_at(dit, "a", 9)
#' dit[["a"]]                                   # 9
#' dat[["a"]]                                   # 1
#' dit.dat = as.dict.table(dat, copy = FALSE)   # init by reference
#' ref_replace_at(dit.dat, "a", 9)
#' dat[["a"]]                                   # 9
#' is.dict.table(dit.dat)                       # TRUE
#' is.dict.table(dat)                           # TRUE now as well!
#'
#' # Coerce from dict
#' d = dict(a = 1, b = 1:3)
#' as.dict.table(d)
#'
as.dict.table.data.table <- function(x, copy = TRUE, ...)
{
    if (copy)
        dict.table(x)
    else
        .set_class(x)
}


#' @export
as.dict.table.default <- function(x, ...)
{
    do.call(dict.table, args = as.list(x))
}

#' @rdname dict.table
#' @details * `is.dict.table(x)` check if `x` is a `dict.table`
#' @export
is.dict.table <- function(x) inherits(x, "dict.table")

#' @rdname dict.table
#' @export
#' @examples
#'
#' # Copy and reference semantics when coercing *to* a data.table
#' dit = dict.table(a = 1, b = 2)
#' dat = as.data.table(dit)
#' is.data.table(dat)                           # TRUE
#' data.table::set(dat, j = "a", value = 9)
#' dat[["a"]]                                   # 9
#' dit[["a"]]                                   # 1
#' dat.dit = as.data.table(dit, copy = FALSE)   # init by reference
#' is.data.table(dat.dit)                       # TRUE
#' is.dict.table(dit)                           # FALSE - not a dict.table anymore ...
#' is.data.table(dit)                           # TRUE  - ... but a data.table
#' data.table::set(dat.dit, j = "a", value = 9)
#' dat.dit[["a"]]                               # 9
#' dit[["a"]]                                   # 9 - also changed
`as.data.table.dict.table` <- function(x, copy = TRUE, ...)
{
    if (copy) {
        data.table::as.data.table(as.list(x), ...)
    } else {
        .remove_class(x, class = "dict.table")
    }
}


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


#' @rdname dict.table
#' @export
#' @examples
#' dit = dict.table(a = 1:2, b = 1:2)
#' rbind(dit, dit)
#'
#' # rbind ...
#' dit = dict.table(a = 1:2, b = 1:2)
#' rbind(dit, dit)
#'
#' # ... can be mixed with data.tables
#' dat = data.table(a = 3:4, b = 3:4)
#' rbind(dit, dat)  # yields a dict.table
#' rbind(dat, dit)  # yields a data.table
rbind.dict.table <- function(x, ...)
{
    dots <- list(...)
    res <- do.call(rbind, args = c(list(as.data.table(x)), dots))
    .set_class(res)
}


#' @rdname dict.table
#' @export
#' @examples
#'
#' # cbind ...
#' dit = dict.table(a = 1:2, b = 1:2)
#' dit2 = dict.table(c = 3:4, d = 5:6)
#' cbind(dit, dit2)
#'
#' # ... can be mixed with data.tables
#' dat = data.table(x = 3:4, y = 3:4)
#' cbind(dit, dat)
cbind.dict.table <- function(x, ...)
{
    dots <- list(...)
    res <- do.call(cbind, args = c(list(as.data.table(x)), dots))

    if (any(duplicated(colnames(res)))) {
        cnames = colnames(res)
        dups = duplicated(cnames)
        stop("found duplicated column names: ",
             toString(cnames[dups]), call. = FALSE)
    }

    .set_class(res)
}

# TODO: implement c.dict.table()

