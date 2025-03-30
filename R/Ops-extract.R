#' Extract Parts of a Container Object
#'
#' @description Extract parts of a `Container` object similar
#' to R's base extract operators on lists.
#' @name OpsExtract
#' @param x `Container` object from which to extract elements.
#' @param i,...  indices specifying elements to extract. Indices
#' can be `logical`, `numeric`, `character`, or a mix of these.
#' They can be passed as `vector`, `list`, or comma-separated
#' sequence.
#' Non-existing indices are ignored, but can be substituted if a
#' default value was provided (see `.default` parameter).
#' Supports alpha-numeric range selection via non-standard evaluation
#' (e.g., `co[a:b]` see Examples).
#'
#' @param .default value to be returned if peeked value does not exist.
#' @details
#' `[` selects multiple values. The indices can be `numeric` or
#' `character` or both. They can be passed as a `vector` or `list` or,
#' for convenience, just as a comma-separated sequence (see Examples).
#' Non-existing indices are ignored.
#'
#' `[[` selects a single value using a `numeric` or `character` index.
#'
#' @section Warning:
#' Alpha-numeric range selection (e.g. `co[a:b]`) is intended for
#' interactive use only, because the non-standard evaluation of the
#' range arguments can have unanticipated consequences.
#' For programming it is therefore better to use the standard subsetting
#' indices (e.g. `co[1:2]` or `co[c("a", "b")]`).
NULL

#' @rdname OpsExtract
#' @examples
#' co = container(a = 1, b = 2, c = 3, d = 4)
#' co[1:2]
#' co[1, 4]
#' co["d", 2]
#' co[list("d", 2)]
#' co[0:10]
#'
#' # Boolean selection
#'
#' # Custom default values
#' co[1:2, 99, .default = 0]
#' co[1:2, "z", .default = -1]
#' co[1:2, "z", .default = 3:4]
#'
#' # Alpha-numeric range selection
#' co[a:b]
#' co[a:b, d:c]
#' co[1:c]
#' co[d:2]
#' co[a:8, .default = a]
#' @export
"[.Container" <- function(x, ...)
{
    args <- as.list(match.call())
    dots <- args[-(1:2)]

    # Handle empty selection
    if (length(dots) == 1) {
        # Make sure empty selection returns x as is
        hasName <- is.name(dots[[1]])
        if (hasName) {
            isEmpty <- nchar(dots[[1]]) == 0
            if (isEmpty) {
                return(x)
            }
        }
    }

    vars <- names(x)
    nl <- as.list(seq_along(vars))
    names(nl) <- vars
    for (i in seq_along(dots)) {
        dots[[i]] <- suppressWarnings(
            # suppress warning about 'restarting interrupted promise evaluation'
            tryCatch(
                list(...)[[i]],
                error = function(e) {
                    # If we get here, we most likley have to handle non-standard
                    # evaluation of alpha-numeric indices like co[a:d]
                    call <- eval(substitute(dots[[i]]), envir = nl)
                    dots[[i]] <- eval(call, envir = nl)
                }
            )
        )
    }

    if (length(dots) == 0) {
        return(peek_at(x, ...))
    }

    # Handle boolean selection
    hasBooleans <- all(sapply(dots, is.logical)) && length(dots) > 0
    if (hasBooleans) {
        mask <- rep(unlist(dots), length.out = length(x))
        indices <- which(mask)
        if (length(indices) == 0) {
            return(container())
        }
        return(peek_at(x, which(mask)))
    }

    res <- suppressWarnings(
        # suppress warning about 'restarting interrupted promise evaluation'
        try(peek_at(x, ...), silent = TRUE)
    )
    ok <- !inherits(res, "try-error")
    if (ok) {
        return(res)
    }

    peek_at(x, dots)
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
