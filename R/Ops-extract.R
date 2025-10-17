
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
    if (.is_call(tok, "(")) {
        return(
            .token_to_piece(tok[[2L]], nm = nm, n = n, env = env, keep_raw = keep_raw)
        )
    }
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


#' Extract Parts of a Container
#'
#' @description
#' Operators that extract parts of a `Container`. The behavior is similar to
#' base R lists and includes convenient extensions for interactive work.
#'
#' @usage
#' x[i, ..., .default = NULL]
#' x[[i]]
#' x$name
#'
#' @name OpsExtract
#'
#' @param x A `Container` from which to extract elements.
#' @param i,... Indices specifying elements to extract. Indices may be numeric,
#'   character, logical, `NULL`, or empty. Logical vectors are recycled as needed.
#'   Negative integers drop by position. Negative character indices drop by name.
#'   Range expressions such as `a:b`, `1:c`, or `d:2` are supported for
#'   convenience and are resolved in the calling environment.
#' @param .default A value used to fill missing items when extracting. If given,
#'   unknown names and out-of-bounds positive indices are kept and filled with
#'   this value.
#' @param name A literal name for `$` extraction.
#'
#' @details
#' The `[` operator selects one or more elements and returns a `Container`.
#' Order is preserved and duplicates are kept. Logical indices recycle to the
#' container length with a warning when lengths do not match. `NA` in logical
#' indices is treated as `FALSE` with a warning. Positive and negative numeric
#' indices cannot be mixed in a single call and will raise an error.
#' Out-of-bounds negative indices are ignored. Character indices match names.
#' Unknown names are ignored unless `.default` is supplied, in which case they
#' are kept and filled. Comma-separated indices and `list(...)` are accepted and
#' behave like a single combined index. `x[]` returns all elements, while
#' `x[NULL]`, `x[i = NULL]`, and `x[foo = NULL]` return an empty container.
#'
#' The `[[` operator selects a single element and returns the value or `NULL`
#' if the element is not present.
#'
#' The `$` operator extracts by name and does not accept computed indices.
#'
#' Range expressions such as `x[a:b]` are intended for interactive use. The
#' endpoints are first matched to names and otherwise evaluated as numeric
#' scalars in the calling environment.
#'
#' @return
#' For `[` a `Container`. For `[[` the extracted value or `NULL`. For `$` the
#' extracted value or `NULL`.
#'
#' @seealso
#' \code{\link{peek_at}} for lenient extraction with defaults,
#' \code{\link{at}} and \code{\link{at2}} for strict programmatic access,
#' and base \code{\link[base:Extract]{[}}, \code{\link[base:Extract]{[[}}, and
#' \code{\link[base:Extract]{$}} for general indexing semantics.
NULL


#' @rdname OpsExtract
#' @examples
#' co <- container(a = 1, b = 2, c = 3, d = 4)
#'
#' # Numeric
#' co[c(1, 4)]                          # [a = 1, d = 4]
#' co[1, 4]                             # same (comma-sugar)
#' co[1, 1]                             # duplicates kept -> [a = 1, a = 1]
#' co[0:5]                              # unknowns ignored -> [a = 1, b = 2, c = 3, d = 4]
#' co[5]                                # [] (unknown positive index)
#'
#' # Negative numeric
#' co[-c(1:2)]                          # [c = 3, d = 4]
#' co[-1, -4]                           # [b = 2, c = 3]
#' try(co[-1, 3])                       # error: cannot mix positive & negative
#' co[-5]                               # out-of-bounds negatives ignored -> full container
#'
#' # Character
#' co[c("a", "d")]                      # [a = 1, d = 4]
#' co["a", "d"]                         # same
#' co[letters[1:5]]                     # unknown names dropped -> [a = 1, b = 2, c = 3, d = 4]
#' co["x"]                              # []
#'
#' # Negative character (drop by name)
#' co[-c("a", "d")]                     # [b = 2, c = 3]
#' co[-"a", -"d"]                       # [b = 2, c = 3]
#'
#' # Logical
#' co[c(TRUE, FALSE, TRUE, FALSE)]      # [a = 1, c = 3]
#' co[TRUE, FALSE]                      # [a = 1, c = 3] (recycled)
#' co[c(TRUE, NA)]                      # [a = 1, c = 3] (NA -> FALSE, warning)
#'
#' # Mixed numeric and character
#' co[list(1, "d")]                     # [a = 1, d = 4]
#' co[1, "d"]                           # same
#'
#' # Alphanumeric ranges (NSE)
#' co[a:b]                              # [a = 1, b = 2]
#' co[a:b, d:c]                         # [a = 1, b = 2, d = 4, c = 3]
#' co[1:c]                              # [a = 1, b = 2, c = 3]
#' co[d:2]                              # [d = 4, c = 3, b = 2]
#' co[-(a:c)]                           # [d = 4]
#'
#' # Default-filling of missing items
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
#' co <- container(a = 1, b = 2, c = 3, d = 4)
#' co[1:2]              # [a = 1, b = 2]
#' co[1, 4]             # [a = 1, d = 4]
#' co[0:10]             # [a = 1, b = 2, c = 3, d = 4]
#' co[list(1, "d")]     # [a = 1, d = 4]
#' co["d", 2]           # same
#' co[-c(1:2)]          # [c = 3, d = 4]
#' co[-1, -4]           # [b = 2, c = 3]
#' co[-"a", -"d"]       # [b = 2, c = 3]
#' co[a:b]              # [a = 1, b = 2]
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
