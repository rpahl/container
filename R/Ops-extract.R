
`%||%` <- function(a, b) if (is.null(a)) b else a

.is_call <- function(x, name) {
    is.call(x) && identical(x[[1L]], as.name(name))
}


.idx_char <- function(chr, nm, keep_raw = FALSE) {
    pos <- match(chr, nm, nomatch = NA_integer_)
    list(
        mode = "positive",
        pos = as.integer(pos[!is.na(pos)]),
        raw = if (keep_raw) lapply(as.list(chr), identity) else NULL
    )
}


.idx_bool <- function(lgl, n, keep_raw = FALSE) {
    if (length(lgl) == 0L) {
        return(list(mode = "positive", pos = integer(), raw = NULL))
    }
    if ((n %% length(lgl)) != 0L) {
        warning(
            sprintf(
                "Logical index of length %d is not a multiple of %d; recycling.",
                length(lgl), n
            ),
            call. = FALSE
        )
    }
    idx <- rep_len(lgl, n)
    if (anyNA(idx)) {
        warning(
            "Logical index contains NA; treating NA as FALSE.",
            call. = FALSE
        )
        idx[is.na(idx)] <- FALSE
    }
    pos <- which(idx)
    list(
        mode = "positive",
        pos = pos,
        raw = if (keep_raw) lapply(as.list(pos), identity) else NULL
    )
}


.idx_num <- function(num, n, keep_raw = FALSE) {
    v <- as.integer(num)
    v <- v[v != 0L & !is.na(v)]
    if (!length(v)) {
        return(list(mode = "positive", pos = integer(), raw = NULL))
    }
    has_pos <- any(v > 0L)
    has_neg <- any(v < 0L)
    if (has_pos && has_neg) {
        stop("cannot mix positive and negative indices.", call. = FALSE)
    }
    if (has_neg) {
        return(list(mode = "negative", pos = abs(v), raw = NULL))
    }
    list(
        mode = "positive",
        pos = v,
        raw = if (keep_raw) lapply(as.list(v), identity) else NULL
    )
}


.endp_pos <- function(endp, nm, n, env) {
    if (is.symbol(endp)) {
        key <- as.character(endp)
        pos <- match(key, nm, nomatch = NA_integer_)
        if (!is.na(pos)) return(pos)
        val <- try(suppressWarnings(eval(endp, env)), silent = TRUE)
        if (is.numeric(val) && length(val) == 1L && is.finite(val)) {
            return(as.integer(val))
        }
        return(NA_integer_)
    }
    val <- try(suppressWarnings(eval(endp, env)), silent = TRUE)
    if (!is.numeric(val) || length(val) != 1L || !is.finite(val)) {
        return(NA_integer_)
    }
    as.integer(val)
}


.value_to_pieces <- function(val, nm, n, keep_raw) {
    if (is.null(val)) {
        return(list(list(mode = "positive", pos = integer(), raw = NULL)))
    }
    if (is.logical(val)) {
        return(list(.idx_bool(val, n, keep_raw)))
    }
    if (is.numeric(val)) {
        return(list(.idx_num(val, n, keep_raw)))
    }
    if (is.character(val)) {
        return(list(.idx_char(val, nm, keep_raw)))
    }
    if (is.list(val)) {
        return(unlist(
            lapply(val, .value_to_pieces, nm = nm, n = n, keep_raw = keep_raw),
            recursive = FALSE
        ))
    }
    list(list(mode = "positive", pos = integer(), raw = NULL))  # ignore other types
}


.token_to_piece <- function(tok, nm, n, env, keep_raw) {
    if (.is_call(tok, "list")) {
        args <- as.list(tok)[-1L]
        return(unlist(
            lapply(args, .token_to_piece, nm = nm, n = n, env = env, keep_raw = keep_raw),
            recursive = FALSE
        ))
    }

    if (.is_call(tok, "-")) {
        inner <- .token_to_piece(tok[[2L]], nm, n, env, keep_raw = FALSE)
        return(lapply(inner, function(p) {
            p$mode <- if (p$mode == "negative") "positive" else "negative"
            p$raw <- NULL
            p
        }))
    }

    if (.is_call(tok, ":")) {
        L <- .endp_pos(tok[[2L]], nm, n, env)
        R <- .endp_pos(tok[[3L]], nm, n, env)
        if (anyNA(c(L, R))) {
            return(list(list(mode = "positive", pos = integer(), raw = NULL)))
        }
        rng <- if (L <= R) seq.int(L, R) else seq.int(L, R, by = -1L)
        return(list(.idx_num(rng, n, keep_raw = keep_raw)))
    }

    if (is.symbol(tok)) {
        key <- as.character(tok)
        if (key %in% nm) {
            return(list(.idx_char(key, nm, keep_raw)))
        }
        val <- try(suppressWarnings(eval(tok, env)), silent = TRUE)
        return(.value_to_pieces(val, nm, n, keep_raw))
    }

    val <- try(suppressWarnings(eval(tok, env)), silent = TRUE)
    .value_to_pieces(val, nm, n, keep_raw)
}


.combine_pieces <- function(pieces, n, keep_raw) {
    if (length(pieces) == 0L) {
        return(list(mode = "positive", pos = seq_len(n)))  # safety
    }

    modes <- vapply(pieces, `[[`, "", "mode")
    if (any(modes == "negative") && any(modes == "positive")) {
        stop("cannot mix positive and negative indices.", call. = FALSE)
    }

    if (any(modes == "negative")) {
        drop_pos <- unique(unlist(lapply(pieces, `[[`, "pos"), use.names = FALSE))
        drop_pos <- drop_pos[drop_pos >= 1L & drop_pos <= n & !is.na(drop_pos)]
        return(list(mode = "negative", pos = setdiff(seq_len(n), drop_pos)))
    }

    if (keep_raw) {
        raw <- list()
        for (p in pieces) {
            if (!is.null(p$raw)) raw <- c(raw, p$raw)
        }
        return(list(mode = "positive", raw_tokens = raw))
    }

    pos <- unlist(lapply(pieces, `[[`, "pos"), use.names = FALSE)
    pos <- pos[pos >= 1L & pos <= n & !is.na(pos)]
    list(mode = "positive", pos = as.integer(pos))
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
`[.Container` <- function(x, i, ..., .default = NULL)
{
    # 1) collect raw tokens (exclude named dots like .default)
    i_expr <- if (missing(i)) NULL else substitute(i)
    dots <- as.list(substitute(list(...)))[-1L]
    if (length(dots) > 0L) {
        nms <- names(dots)
        if (!is.null(nms)) dots <- dots[is.na(nms) | nms == ""]
    }
    toks <- c(if (!is.null(i_expr)) list(i_expr) else NULL, dots)
    if (length(toks) == 0L) {
        if (!missing(i) || !missing(...)) {
            return(x[0])
        }
        return(x)
    }
    if (length(toks) == 1L && .is_call(toks[[1L]], "list")) {
        toks <- as.list(toks[[1L]])[-1L]
    }

    n <- length(x)
    nm <- names(x) %||% rep.int("", n)
    env <- parent.frame()
    keep_raw <- !is.null(.default)

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
    if (identical(indices$mode, "negative")) {
        out <- x[indices$pos]
    } else if (keep_raw) {
        args <- c(list(x), indices$raw_tokens)
        args[[".default"]] <- .default
        out <- do.call(peek_at, args)
    } else {
        out <- peek_at(x, indices$pos)
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
