#' Replace Parts of a Container
#'
#' @description Replace parts of a `Container` object similar
#' to R's base replace operators on lists.
#' @name OpsReplace
#' @param x `Container` object in which to replace elements.
#' @param i  indices specifying elements to replace. Indices
#' are `numeric` or `character` vectors or a `list` containing both.
#' @param ... additional indices (comma-separated) supporting the same
#'   options as extraction: numeric, character, logical, list(...),
#'   ranges (e.g., a:b, 1:c, d:2), unary minus for negative (drop) selection,
#'   and parentheses for grouping.
#' @param name `character` string (possibly backtick quoted)
#' @param value the replacing value of `ANY` type
#' @details
#'  `[<-` replaces multiple values. The indices can be `numeric` or
#' `character` or both. They can be passed as a `vector` or `list`. Values can
#' be added by 'replacing' at new indices, which only works for `character`
#' indices.
#'
#' `[[<-` replaces a single value at a given `numeric` or `character` index.
#' Instead of an index, it is also possible to replace certain elements by
#' passing the element in curly braces (see Examples), that is, the object is
#' searched for the element and then the element is replaced by the value.
#'
#' `$<-` replaces a single element at a given name.
NULL


#' @rdname OpsReplace
#' @examples
#' co = container(a = 1, b = "bar")
#' (co[1:2] <- 1:2)
#'
#' try({
#' co[3] <- 3 # index out of range
#' })
#' (co[list(1, "b")] <- 3:4)   # mixed numeric/character index
#' co[-(1:2)] <- 0             # negative (complement) selection
#' co[a:b] <- list(7, 8)       # range by names (NSE)
#' co["x"] <- 9                # add by new character name
#'
#' @export
"[<-.Container" = function(x, i, ..., value)
{
    # 1) collect raw tokens (exclude named dots like .default)
    i_expr <- if (missing(i)) NULL else substitute(i)
    dots <- as.list(substitute(list(...)))[-1L]
    if (length(dots) > 0L) {
        nms <- names(dots)
        if (!is.null(nms)) dots <- dots[is.na(nms) | nms == ""]
    }
    toks <- c(if (!is.null(i_expr)) list(i_expr) else NULL, dots)
    #if (length(toks) == 0L) {
    #    if (!missing(i) || !missing(...)) {
    #        return(x[0])
    #    }
    #    return(x)
    #}
    if (length(toks) == 1L && .is_call(toks[[1L]], "list")) {
        toks <- as.list(toks[[1L]])[-1L]
    }

    n <- length(x)
    nm <- names(x) %||% rep.int("", n)
    env <- parent.frame()
    keep_raw <- TRUE

    # 2) special case: all tokens are logical -> concatenate, then index once
    all_logical <- length(toks) > 1L && {
        vals <- lapply(
            toks,
            FUN = function(e) try(suppressWarnings(eval(e, env)), silent = TRUE)
        )
        all(vapply(vals, is.logical, logical(1)))
    }
    pieces <- if (all_logical) {
        lgl <- unlist(lapply(toks, function(e) eval(e, env)), use.names = FALSE)
        list(.idx_bool(lgl, n, keep_raw = keep_raw))
    } else {
        unlist(
            lapply(
                toks, FUN = .token_to_piece,
                nm = nm, n = n, env = env, keep_raw = keep_raw
            ),
            recursive = FALSE
        )
    }

    # 3) get combine indices and defer to peek_at
    indices <- .combine_pieces(pieces, n, keep_raw)
    #pos <- indices$pos
    pos <- indices$raw_tokens
    #browser()

    lenv <- length(value)
    leni <- length(pos)
    if (leni < lenv || leni %% lenv != 0)
        warning("number of items to replace (", leni,
                ") is not a multiple of replacement length (", lenv, ")")

    value <- rep(value, length.out = length(pos))
    ref_replace_at(x, pos, value, .add = TRUE)
}

#' @name ContainerS3
#' @rdname ContainerS3
#' @examples
#' co = container(a = 1, b = "bar")
#' (co[1:2] <- 1:2)
#'
#' try({
#' co[3] <- 3 # index out of range
#' })
#' (co[list(1, "b")] <- 3:4)   # mixed numeric/character index
#'
NULL



#' @rdname OpsReplace
#' @examples
#' co = container(a = 1, b = 2)
#' co[[1]] <- 9
#' co[["b"]] <- 8
#' co[["x"]] <- 7
#' co$z <- 99
#' print(co)
#'
#' # Replace 8 by 0
#' co[[{8}]] <- 0
#' print(co)
#'
#' @export
"[[<-.Container" = function(x, i, value)
{
    isub = substitute(i)
    char1 = as.character(isub)[1]

    i_is_value = startsWith(trimws(char1), prefix = "{")

    if (i_is_value)
        return(ref_replace(x, old = i, new = value))


    if (length(i) != 1)
        stop("index must be of length 1", call. = FALSE)

    ref_replace_at(x, i, value, .add = TRUE)
}

#' @name ContainerS3
#' @rdname ContainerS3
#' @examples
#' co = container(a = 1, b = 2)
#' co[[1]] <- 9
#' co[["b"]] <- 8
#' co[["x"]] <- 7
#' co$z <- 99
#' print(co)
#'
#' # Replace 8 by 0
#' co[[{8}]] <- 0
#' print(co)
#'
NULL


#' @rdname OpsReplace
#' @examples
#'
#' co = container(a = 1, b = "bar")
#' co$f <- 3
#' co$b <- 2
#' co
#'
#' @export
"$<-.Container" = function(x, name, value)
{
    ref_replace_at(x, name, value, .add = TRUE)
}

#' @name ContainerS3
#' @rdname ContainerS3
#' @examples
#'
#' co = container(a = 1, b = "bar")
#' co$f <- 3
#' co$b <- 2
#' co
#'
NULL
