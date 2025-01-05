#' Extract Parts of a Container Object
#'
#' @description Extract parts of a `Container` object similar
#' to R's base extract operators on lists.
#' @name OpsExtract
#' @param x `Container` object from which to extract elements.
#' @param i,...  indices specifying elements to extract. Indices
#' are `numeric` or `character` vectors or a `list` containing both.
#' @details
#' `[` selects multiple values. The indices can be `numeric` or
#' `character` or both. They can be passed as a `vector` or `list` or,
#' for convenience, just as a comma-separated sequence (see Examples).
#' Non-existing indices are ignored.
#'
#' `[[` selects a single value using a `numeric` or `character` index.
NULL

#' @rdname OpsExtract
#' @examples
#' co = container(a = 1, b = 2, c = 3, d = 4)
#' co[1:2]
#' co[1, 4]
#' co["d", 2]
#' co[list("d", 2)]
#' co[0:10]
#' @export
"[.Container" <- function(x, ...)
{
    args <- as.list(match.call())
    dots <- args[-(1:2)]

    isEmpty = length(dots) == 1 && is.name(dots[[1]])
    if (isEmpty) {
        return(x)
    }

    selects <- sapply(
        dots,
        FUN = function(.) {
            .eval_range_select(vars = names(x), select = eval(.))
        },
        simplify = FALSE
    )

    hasBooleanSelection = all(sapply(selects, is.logical))
    if (hasBooleanSelection) {
        mask = rep(unlist(selects), length.out = length(x))
        indices = which(mask)
        return(peek_at(x, indices))
    }

    do.call(peek_at, args = c(list(.x = x), selects))
}


#' @name ContainerS3
#' @rdname ContainerS3
#' @examples
#' # Extract or replace
#' co = container(a = 1, b = 2, c = 3, d = 4)
#' co[1:2]
#' co[1, 4]
#' co["d", 2]
#' co[list("d", 2)]
#' co[0:10]
NULL


#' @rdname OpsExtract
#' @examples
#'
#' co = container(a = 1, b = 2)
#' co[[1]]
#' co[["a"]]
#' co[["x"]]
#' @export
"[[.Container" <- function(x, i)
{
    x$peek_at2(i)
}

#' @name ContainerS3
#' @rdname ContainerS3
#' @examples
#'
#' co = container(a = 1, b = 2)
#' co[[1]]
#' co[["a"]]
#' co[["x"]]
NULL
