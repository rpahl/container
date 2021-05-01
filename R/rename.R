.rename_check_names = function(.x, old, new)
{
    if (!is.character(old)) stop("'old' must be character")
    if (!is.character(new)) stop("'new' must be character")

    if (length(old) != length(new))
        stop("'old' and 'new' names must be of the same length")

    if (any(duplicated(old)))
        stop("'old' has duplicated names: ", toString(old[duplicated(old)]))

    if (any(duplicated(new)))
        stop("'new' has duplicated names: ", toString(new[duplicated(new)]))

    if (any(!(hasName(.x, old))))
        stop("Items of 'old' not found in names: ",
             toString(Filter(old, f = function(name) !hasName(.x, name))))

    invisible()
}

#' Rename elements safely
#'
#' @description Search for old name and replace it by new name. If either the
#' old name does not exist or the name would result in a name-clash with an
#' already existing name, an error is signaled.
#' @details The passed old and new names can be vectors but always must have
#' the same length and must be unique to prevent double-renaming.
#' @param .x any `R` object with names.
#' @param old `character` vector of old names.
#' @param new `character` vector of new names.
#' @param ... additional arguments to be passed to or from methods.
#' @return For standard `R` vectors renames `old` to `new` and returns the
#' renamed vector.
#' @details `rename` uses copy semantics while `rename_` works by reference.
#' @export
rename <- function(.x, old, new, ...)
{
    .rename_check_names(.x, old, new)
    UseMethod("rename")
}


#' @rdname rename
#' @export
rename_ <- function(.x, old, new, ...)
{
    .rename_check_names(.x, old, new)
    UseMethod("rename_")
}


#' @rdname rename
#' @return For `Dict` renames key `old` to `new` in place (i.e. by reference)
#' and invisibly returns the [Dict()] object.
#' @examples
#'
#' # Dict
#' d = dict(a = 1, b = 2, c = 3)
#' (rename(d, c("a", "b"), c("a1", "y")))
#' @export
rename.Dict <- function(.x, old, new)
{
    (rename_(.x$clone(deep = TRUE), old, new))
}

#' @name rename.Dict
#' @rdname DictS3
#' @usage
#' rename(.x, old, new)
#' rename_(.x, old, new)
#' @details
#' `rename(.x, old, new)` and `rename_(.x, old, new)` rename one or more keys
#' from `old` to `new`, respectively, by copy and in place (i.e. by reference).
#' @examples
#' d = dict(a = 1, b = 2, c = 3)
#' (rename(d, c("a", "b"), c("a1", "y")))
NULL

#' @name rename
#' @export
rename_.Dict <- function(.x, old, new)
{
    .x$rename(old, new)
}



#' @rdname rename
#' @return For `dict.table` renames key `old` to `new` in place (i.e. by
#' reference) and invisibly returns the [dict.table()] object.
#' @export
#' @examples
#'
#' # dict.table
#' dit = dict.table(a = 1, b = 2, c = 3)
#' (rename(dit, c("a", "b"), c("a1", "y")))
#' rename_(dit, c("a", "b"), c("a1", "y"))
#' print(dit)
rename.dict.table <- function(.x, old, new, ...)
{
    (rename_(copy(.x), old, new, ...))
}


#' @name rename.dict.table
#' @param ... further arguments passed to [data.table::setnames()]
#' @rdname dict.table
#' @usage
#' rename(.x, old, new, ...)
#' rename_(.x, old, new, ...)
#' @details
#' `rename(.x, old, new)` and `rename_(.x, old, new)` rename one or more keys
#' from `old` to `new`, respectively, by copy and in place (i.e. by reference).
#' @examples
#' dit = dict.table(a = 1, b = 2, c = 3)
#' (rename(dit, c("a", "b"), c("a1", "y")))
#' rename_(dit, c("a", "b"), c("a1", "y"))
#' print(dit)
NULL

#' @name rename
#' @export
rename_.dict.table <- function(.x, old, new, ...)
{
    data.table::setnames(.x, old, new, ...)
}



#' @export
rename.default <- function(.x, old, new, ...)
{
    if (!is.vector(.x)) {
        stop("no applicable method for 'rename' applied to an object ",
             "of class '", data.class(.x), "'")
    }

    pos <- match(old, names(.x))
    names(.x)[pos] <- new
    .x
}

