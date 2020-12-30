#' Rename elements safely
#'
#' @description Search for old name and replace it by new name. If either the
#' old name does not exist or the name would result in a name-clash with an
#' already existing name, an error is signaled.
#' @details The passed old and new names can be vectors but always must have
#' the same length and must be unique to prevent double-renaming.
#' @param x any `R` object with names.
#' @param old `character` vector of old names.
#' @param new `character` vector of new names.
#' @param ... additional arguments to be passed to or from methods.
#' @return For standard `R` vectors renames `old` to `new` and returns the
#' renamed vector.
#' @export
rename <- function(x, old, new, ...)
{
    stopifnot(is.character(old))
    stopifnot(is.character(new))
    stopifnot(length(old) == length(new))
    if (any(duplicated(old))) {
        stop("'old' has duplicated names: ", toString(old[duplicated(old)]))
    }
    if (any(duplicated(new))) {
        stop("'new' has duplicated names: ", toString(new[duplicated(new)]))
    }

    UseMethod("rename")
}

#' @rdname rename
#' @return For `Dict` renames key `old` to `new` in place and invisibly returns
#' the [Dict()] object.
#' @export
rename.Dict <- function(x, old, new) x$rename(old, new)


#' @export
rename.default <- function(x, old, new)
{
    if (!is.vector(x)) {
        stop("no applicable method for 'rename' applied to an object ",
             "of class '", data.class(x), "'")
    }

    if (!all(old %in% names(x))) {
        stop("name(s) not found: ", toString(old[!(old %in% names(x))]))
    }

    pos <- match(old, names(x))
    names(x)[pos] <- new
    x
}

