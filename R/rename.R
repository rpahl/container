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

    if (any(!(utils::hasName(.x, old))))
        stop("Items of 'old' not found in names: ",
             toString(Filter(old, f = function(name) !utils::hasName(.x, name))))

    invisible()
}

#' Rename Elements Safely
#'
#' @description Search for old name and replace it by new name. If either the
#' old name does not exist or the name would result in a name-clash with an
#' already existing name, an error is signaled.
#' @details The passed old and new names can be vectors but always must have
#' the same length and must be unique to prevent double-renaming.
#' @param .x any `R` object with names.
#' @param old `character` vector of old names.
#' @param new `character` vector of new names.
#' @return For standard `R` vectors renames `old` to `new` and returns the
#' renamed vector.
#' @details `rename` uses copy semantics while `ref_rename` works by reference,
#' that is, it renames elements in place.
#' @export
rename <- function(.x, old, new)
{
    .rename_check_names(.x, old, new)
    UseMethod("rename")
}


#' @rdname rename
#' @export
ref_rename <- function(.x, old, new)
{
    .rename_check_names(.x, old, new)
    UseMethod("ref_rename")
}


#' @name ContainerS3
#' @rdname ContainerS3
#' @details
#' * `rename(.x, old, new)` and `ref_rename(.x, old, new)` rename one or more keys
#' from `old` to `new`, respectively, by copy and in place (i.e. by reference).
#' @examples
#' co = container(a = 1, b = 2, 3)
#' rename(co, c("a", "b"), c("a1", "y"))
#' print(co)
#' ref_rename(co, c("a", "b"), c("a1", "y"))
#' print(co)
NULL

#' @rdname rename
#' @return For `Container`, an object of class `Container` (or one of the
#' respective derived classes).
#' @examples
#'
#' # Container
#' co = container(a = 1, b = 2, 3)
#' rename(co, c("a", "b"), c("a1", "y"))
#' print(co)
#' ref_rename(co, c("a", "b"), c("a1", "y"))
#' print(co)
#' @export
rename.Container <- function(.x, old, new)
{
    (ref_rename(.x$clone(deep = TRUE), old, new))
}

#' @export
ref_rename.Container <- function(.x, old, new)
{
    .x$rename(old, new)
}



#' @rdname rename
#' @param .x `dict.table` object
#' @param old `character` old name
#' @param new `character` new name
#' @return For `dict.table` renames key `old` to `new` in place (i.e. by
#' reference) and invisibly returns the [dict.table()] object.
#' @export
#' @examples
#'
#' # dict.table
#' dit = dict.table(a = 1, b = 2, c = 3)
#' rename(dit, c("a", "b"), c("a1", "y"))
#' print(dit)
#' ref_rename(dit, c("a", "b"), c("a1", "y"))
#' print(dit)
rename.dict.table <- function(.x, old, new)
{
    (ref_rename(copy(.x), old, new))
}


#' @rdname rename
#' @export
ref_rename.dict.table <- function(.x, old, new)
{
    data.table::setnames(.x, old, new)
}


#' @name dicttable
#' @rdname dicttable
#' @details
#' * `rename(.x, old, new)` and `ref_rename(.x, old, new)` rename one or more
#' columns from `old` to `new`, respectively, by copy and in place (i.e. by
#' reference).
#' @examples
#'
#' dit = dict.table(a = 1, b = 2, c = 3)
#' rename(dit, c("a", "b"), c("a1", "y"))
#' print(dit)
#' ref_rename(dit, c("a", "b"), c("a1", "y"))
#' print(dit)
NULL


#' @rdname rename
#' @export
rename.default <- function(.x, old, new)
{
    if (!is.vector(.x)) {
        stop("no applicable method for 'rename' applied to an object ",
             "of class '", data.class(.x), "'")
    }

    pos <- match(old, names(.x))
    names(.x)[pos] <- new
    .x
}

