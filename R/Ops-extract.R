
.is_call <- function(x, name) is.call(x) && identical(x[[1L]], as.name(name))

# Expand an endpoint (symbol or number) to a 1-based position
.get_endpoint_pos <- function(endp, nm, n, env)
{
    if (is.symbol(endp)) {
        key <- as.character(endp)
        pos <- match(key, nm, nomatch = NA_integer_)
        if (is.na(pos)) {
            # fall back to evaluating the symbol (allows 1:c where c is a
            # variable)
            val <- try(suppressWarnings(eval(endp, env)), silent = TRUE)

            pos <- if (is.numeric(val) && length(val) == 1L && is.finite(val)) {
                as.integer(val)
            } else {
                # ignore silently
                NA_integer_
            }
        }
        return(pos)
    }

    # numeric literal / expression
    val <- try(suppressWarnings(eval(endp, env)), silent = TRUE)
    if (!is.numeric(val) || length(val) != 1L || !is.finite(val)) {
        return(NA_integer_)
    }
    as.integer(val)
}

# Turn a single token (language/object) into positions + negate flag
.token_to_pos <- function(tok, nm, n, env, keep_raw)
{
    res <- list(pos = integer(0L), raw = NULL, negate = FALSE)
    if (is.null(tok)) return(res)

    if (is.call(tok) && identical(tok[[1L]], as.name("list"))) {
        args <- as.list(tok)[-1L]
        out <- lapply(
            args,
            FUN = .token_to_pos,
            nm = nm, n = n, env = env, keep_raw = keep_raw
        )
        return(list(pos = out, raw = NULL, negate = "LIST"))
    }

    if (is.call(tok) && identical(tok[[1L]], as.name("-"))) {
        inner <- .token_to_pos(tok[[2L]], nm, n, env, keep_raw = FALSE)
        if (identical(inner$negate, "LIST")) {
            inner_flat <- .flatten_list_nodes(
                inner$pos, nm, n, env, keep_raw = FALSE
            )
            inner <- list(pos = inner_flat$pos, raw = NULL, negate = FALSE)
        }
        inner$negate <- !isTRUE(inner$negate)
        return(inner)
    }

    if (is.call(tok) && identical(tok[[1L]], as.name(":"))) {
        lhs <- tok[[2L]]
        rhs <- tok[[3L]]
        L <- .get_endpoint_pos(lhs, nm, n, env)
        R <- .get_endpoint_pos(rhs, nm, n, env)
        rng <- if (L <= R) seq.int(L, R) else seq.int(L, R, by = -1L)
        res$pos <- as.integer(rng)
        # numeric raw tokens
        if (keep_raw) res$raw <- as.list(as.integer(rng))
        return(res)
    }

    if (is.symbol(tok)) {
        key <- as.character(tok)
        if (key %in% nm) {
            res$pos <- match(key, nm)
            # name token as character
            if (keep_raw) res$raw <- list(key)
            return(res)
        }
    }

    if (is.call(tok) && identical(tok[[1L]], as.name("c"))) {
        val <- try(suppressWarnings(eval(tok, env)), silent = TRUE)
        return(.value_to_pos(val, nm, n, keep_raw))
    }

    val <- try(suppressWarnings(eval(tok, env)), silent = TRUE)
    .value_to_pos(val, nm, n, keep_raw)
}

.value_to_pos <- function(val, nm, n, keep_raw)
{
    res <- list(pos = integer(0L), raw = NULL, negate = FALSE)
    if (is.null(val)) return(res)

    if (is.logical(val)) {
        L <- length(val)
        if (L == 0L) return(res)
        if ((n %% L) != 0L) {
            warning(sprintf(
                "Logical index of length %d is not a multiple of %d; recycling.",
                L, n), call. = FALSE)
        }
        idx <- rep_len(val, n)
        if (anyNA(idx)) {
            warning("Logical index contains NA; treating NA as FALSE.",
                    call. = FALSE)
        }
        idx[is.na(idx)] <- FALSE
        res$pos <- which(idx)
        return(res)
    }

    if (is.numeric(val)) {
        v <- as.integer(val)
        v <- v[v != 0L & !is.na(v)]
        has_pos <- any(v > 0L)
        has_neg <- any(v < 0L)
        if (has_pos && has_neg) {
            stop("cannot mix positive and negative indices.", call. = FALSE)
        }
        if (has_neg) return(list(pos = abs(v), raw = NULL, negate = TRUE))
        res$pos <- v
        # list of integers
        if (keep_raw) res$raw <- lapply(v, identity)
        return(res)
    }

    if (is.character(val)) {
        if (keep_raw) {
            # list of names
            res$raw <- lapply(as.list(val), identity)
            known <- match(val, nm, nomatch = NA_integer_)
            res$pos <- known[!is.na(known)]
            return(res)
        } else {
            pos <- match(val, nm, nomatch = NA_integer_)
            if (anyNA(pos)) {
                missing <- unique(val[is.na(pos)])
                pos <- pos[!is.na(pos)]
            }
            res$pos <- as.integer(pos)
            return(res)
        }
    }

    if (is.list(val)) {
        parts <- lapply(val, .value_to_pos, nm = nm, n = n, keep_raw = keep_raw)
        return(list(pos = parts, raw = NULL, negate = "LIST"))
    }

    res
}

.flatten_list_nodes <- function(nodes, nm, n, env, keep_raw)
{
    out_pos <- list()
    out_raw <- list()
    out_neg <- logical()
    for (node in nodes) {
        if (identical(node$negate, "LIST")) {
            inner <- .flatten_list_nodes(node$pos, nm, n, env, keep_raw)
            out_pos <- c(out_pos, list(list(pos = inner$pos, negate = FALSE)))
            out_raw <- c(out_raw, inner$raw)
            out_neg <- c(out_neg, FALSE)
        } else {
            out_pos <- c(out_pos, list(node))
            # node$raw is already a list of scalars
            if (!is.null(node$raw)) out_raw <- c(out_raw, node$raw)
            out_neg <- c(out_neg, isTRUE(node$negate))
        }
    }
    list(
        pos     = unlist(lapply(out_pos, `[[`, "pos"), use.names = FALSE),
        raw     = out_raw,   # keep as list (no unlist!)
        any_neg = any(out_neg),
        all_neg = all(out_neg)
    )
}


# Top-level resolver: returns final positive positions
# (1-based, duplicates allowed)
.resolve_indices <- function(
    x, i_expr, dots_exprs, env, keep_raw
) {
    n  <- length(x)
    nm <- names(x) %||% rep.int("", n)

    toks <- c(if (!is.null(i_expr)) list(i_expr) else list(), dots_exprs)
    if (length(toks) == 1L && .is_call(toks[[1L]], "list")) {
        toks <- as.list(toks[[1L]])[-1L]
    }

    all_logical <- length(toks) > 1L && all(vapply(toks, function(e) {
        v <- try(suppressWarnings(eval(e, env)), silent = TRUE)
        is.logical(v)
    }, logical(1)))

    if (all_logical) {
        v <- unlist(lapply(toks, function(e) eval(e, env)), use.names = FALSE)
        nodes <- list(.value_to_pos(v, nm, n, keep_raw = FALSE))
    } else {
        nodes <- lapply(
            toks,
            FUN = .token_to_pos,
            nm = nm, n = n, env = env, keep_raw = keep_raw

        )
    }

    # flatten any LIST markers
    if (any(vapply(nodes, function(z) identical(z$negate, "LIST"),
                   logical(1)))) {
        flat <- .flatten_list_nodes(
            lapply(nodes, function(z) {
                if (identical(z$negate, "LIST")) z$pos else z
            }),
            nm, n, env, keep_raw
        )
        nodes <- list(list(pos = flat$pos, raw = flat$raw, negate = FALSE))
    }

    vals      <- lapply(nodes, `[[`, "pos")
    raws      <- lapply(nodes, `[[`, "raw")
    negatives <- vapply(nodes, function(z) isTRUE(z$negate), logical(1))

    if (any(negatives)) {
        if (!all(negatives)) {
            stop("cannot mix positive and negative indices.", call. = FALSE)
        }
        drop_pos <- as.integer(unlist(vals, use.names = FALSE))
        drop_pos <- unique(
            drop_pos[drop_pos >= 1L & drop_pos <= n & !is.na(drop_pos)]
        )
        sel <- setdiff(seq_len(n), drop_pos)
        return(list(mode = "negative", pos = as.integer(sel)))
    }

    if (keep_raw) {
        # concatenate lists of scalar tokens, preserving types
        raw_tokens <- list()
        for (r in raws) if (!is.null(r)) raw_tokens <- c(raw_tokens, r)
        return(list(mode = "positive", raw_tokens = raw_tokens))
    } else {
        sel <- as.integer(unlist(vals, use.names = FALSE))
        sel <- sel[sel >= 1L & sel <= n & !is.na(sel)]
        return(list(mode = "positive", pos = sel))
    }
}



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
#' co <- container(a = 1, b = 2, c = 3, d = 4)
#'
#' # Numeric
#' co[c(1, 4)]                          # [a = 1, d = 4]
#' co[1, 4]                             # same
#' co[0:5]                              # [a = 1, b = 2, c = 3, d = 4]
#' co[5]                                # []
#'
#' # Negative numeric
#' co[-c(1:2)]                          # [c = 3, d = 4]
#' co[-1, -4]                           # [b = 2, c = 3]
#' try(co[-1, 3])                       # cannot mix positive and negative indices
#'
#' # Character
#' co[c("a", "d")]                      # [a = 1, d = 4]
#' co["a", "d"]                         # same
#' co[letters[1:5]]                     # [a = 1, b = 2, c = 3, d = 4]
#' co["x"]                              # []
#'
#' # Negative character
#' co[-c("a", "d")]                      # [b = 2, c = 3]
#' co[-"a", -"d"]                        # [b = 2, c = 3]
#'
#' # Boolean
#' co[c(TRUE, FALSE, TRUE, FALSE)]      # [a = 1, c = 3]
#' co[TRUE, FALSE, TRUE, FALSE]         # same
#'
#' # Partial boolean (recycling)
#' co[c(TRUE, FALSE)]                   # [a = 1, c = 3]
#' co[TRUE, FALSE]                      # same
#'
#' # Mixed numeric and character
#' co[list(1, "d")]                     # [a = 1, d = 4]
#' co[1, "d"]                           # same
#'
#' # Alpha-numeric ranges (non-standard evaluation)
#' co[a:b]                              # [a = 1, b = 2]
#' co[a:b, d:c]                         # [a = 1, b = 2, d = 4, c = 3]
#' co[1:c]                              # [a = 1, b = 2, c = 3]
#' co[d:2]                              # [d = 4, c = 3]
#'
#' # Default values
#' co[1:5, 0, .default = 0]             # [a = 1, b = 2, c = 3, d = 4, 0]
#' co["a", "b", "z", .default = 0]      # [a = 1, b = 2, z = 0]
#' co[1:2, "z", .default = 3:4]         # [a = 1, b = 2, z = (3L 4L)]
#'
#' @export
`[.Container` <- function(
    x, i, ...,
    .default = NULL
) {
    i_expr     <- if (missing(i)) NULL else substitute(i)
    dots_exprs <- as.list(substitute(list(...)))[-1L]

    if (!missing(i) && is.null(i_expr)) {
        return(x[0])
    }

    if (length(i_expr) == 0L && length(dots_exprs) == 0L) {
        # No indices: return all
        return(x)
    }

    keep_raw <- !is.null(.default)

    idx <- .resolve_indices(
        x, i_expr, dots_exprs,
        env = parent.frame(),
        keep_raw = keep_raw
    )

    if (identical(idx$mode, "negative")) {
        out <- x[idx$pos]
    } else if (keep_raw) {
        # list of scalars, types preserved
        args <- c(list(x), idx$raw_tokens)
        args[[".default"]] <- .default
        out <- do.call(peek_at, args)
    } else {
        out <- peek_at(x, idx$pos)
    }

    class(out) <- class(x)
    out
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
"[[.Container" <- function(x, i) {
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
