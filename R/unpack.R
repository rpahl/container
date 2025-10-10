#' Unpack Nested Objects
#'
#' Similary to [unlist()] recursively unpacks any (possibly nested) structure
#' into a flat list. In contrast to [unlist()], [unpack()] also works with
#' (possibly nested) [Container()] objects. In principle, it works for any
#' object that can be transformed to a list via `as.list`.
#' @param x any `R` object
#' @param recursive `logical` descend recursively into nested objects?
#' @param use.names `logical` Should names be preserved?
#' @return a `list`
#' @export
unpack = function(x, recursive = TRUE, use.names = TRUE) {

    if (!is.recursive(x))
        return(x)

    unpack_recursive = function(x) {
        if (is.container(x))
            rapply(as.list(x), f = unpack_recursive)
        else
            unlist(x, recursive = TRUE, use.names = use.names)
    }

    unpack_nonrecursive = function(x) {
        if (is.container(x))
            unlist(as.list(x), recursive = FALSE, use.names = use.names)
        else
            unlist(x,          recursive = FALSE, use.names = use.names)
    }

    res = if (recursive)
        rapply(as.list(x), f = unpack_recursive)
    else
        unpack_nonrecursive(x)

    if (!use.names)
        names(res) = NULL

    res
}
