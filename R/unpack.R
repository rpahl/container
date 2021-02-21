#' Unpack nested objects
#'
#' Similary to [unlist()] recursively unpacks any (possibly nested) structure
#' into a flat list. In contrast to [unlist()], [unpack()] also works with
#' (possibly nested) [Container()] objects.
#' @param x any `R` object
#' @return a `list`
#' @export
unpack = function(x) {

    .unpack = function(x) {
        if (is.container(x))
            rapply(as.list(x), f = .unpack)
        else
            unlist(x)
    }

    if (is.recursive(x))
        rapply(as.list(x), f = .unpack)
    else
        x
}

