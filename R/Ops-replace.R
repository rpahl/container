#' Replace Parts of a Container
#'
#' @description Replace parts of a `Container` object similar to R's base list
#' replacement operators, with extended indexing options matching extraction.
#' @name OpsReplace
#' @param x `Container` object in which to replace elements.
#' @param i,... Indices specifying elements to replace. Indices may be numeric,
#'   character, logical, `NULL`, or empty. Logical vectors are recycled as needed.
#'   Negative integers and negative character tokens select the complement (i.e.,
#'   drop-by-position or drop-by-name) and the resulting kept positions are
#'   replaced. Range expressions such as `a:b`, `1:c`, or `d:2` are supported for
#'   convenience and are resolved in the calling environment (non-standard
#'   evaluation). Comma-separated indices and `list(...)` are accepted and behave
#'   like a single combined index.
#' @param name `character` string (possibly backtick quoted)
#' @param value the replacing value of `ANY` type
#' @details
#' * `[<-` replaces multiple values. Indices can be numeric, character, logical,
#'   or a list combining them, including NSE ranges as in extraction. Unknown
#'   character indices add new elements (equivalent to `.add = TRUE`). Numeric
#'   indices must be within bounds and will error if out of range. Using an empty
#'   index `x[] <- v` targets all positions. Zero-length selections (e.g.,
#'   `integer(0)`) perform no replacement.
#'
#' * `[[<-` replaces a single value at a given numeric or character index. Instead
#'   of an index, it is also possible to replace certain elements by passing the
#'   element in curly braces (see Examples), that is, the object is searched for
#'   the element and then the element is replaced by the value.
#'
#' * `$<-` replaces a single element at a given name.
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
#'
#' @export
"[<-.Container" = function(x, i, ..., value)
{
    # Capture tokens (exclude any named dots intended for other args)
    i_expr <- if (missing(i)) NULL else substitute(i)
    dots <- as.list(substitute(list(...)))[-1L]
    if (length(dots) > 0L) {
        nms <- names(dots)
        if (!is.null(nms)) dots <- dots[is.na(nms) | nms == ""]
    }
    toks <- c(if (!is.null(i_expr)) list(i_expr) else NULL, dots)

    n <- length(x)
    env <- parent.frame()

    # co[] <- value targets all positions; zero-length selections do nothing
    if (length(toks) == 0L) {
        idx_input <- if (n > 0L) seq_len(n) else integer()
    } else {
        if (length(toks) == 1L && .is_call(toks[[1L]], "list")) {
            toks <- as.list(toks[[1L]])[-1L]
        }
        indices <- .get_pos_indices(x, keep_raw = TRUE, .env = env, .toks = toks)
        if (!is.null(indices$raw_tokens)) {
            # Mixed/positive/char/logical cases -> use raw tokens list to allow adds
            idx_input <- indices$raw_tokens
        } else {
            # Negative/complement case -> numeric positions to keep
            idx_input <- as.integer(indices$pos)
        }
    }

    leni <- length(idx_input)
    if (leni == 0L) return(invisible(x))

    lenv <- length(value)
    if (leni < lenv || leni %% lenv != 0) {
        warning(
            "number of items to replace (", leni,
            ") is not a multiple of replacement length (", lenv, ")",
            call. = FALSE
        )
    }
    value <- rep(value, length.out = leni)

    # Pair indices with values: pass as two-arg form to ref_replace_at
    ref_replace_at(x, idx_input, value, .add = TRUE)
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
