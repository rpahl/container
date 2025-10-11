
.is_call <- function(x, name) is.call(x) && identical(x[[1L]], as.name(name))

# Expand an endpoint (symbol or number) to a 1-based position
.container_endpoint_pos <- function(endp, nm, n, env, strict) {
  if (is.symbol(endp)) {
    key <- as.character(endp)
    pos <- match(key, nm, nomatch = NA_integer_)
    if (is.na(pos)) {
      if (strict) stop(sprintf("Unknown name in range: %s", key), call. = FALSE)
      # fall back to evaluating the symbol (allows 1:c where c is a variable)
      val <- try(suppressWarnings(eval(endp, env)), silent = TRUE)
      if (is.numeric(val) && length(val) == 1L && is.finite(val)) {
        pos <- as.integer(val)
      } else {
        # ignore silently if non-strict
        pos <- NA_integer_
      }
    }
    return(pos)
  }
  # numeric literal / expression
  val <- try(suppressWarnings(eval(endp, env)), silent = TRUE)
  if (!is.numeric(val) || length(val) != 1L || !is.finite(val)) {
    if (strict) stop("Range endpoints must resolve to single finite numbers or names.", call. = FALSE)
    return(NA_integer_)
  }
  as.integer(val)
}

# Turn a single token (language/object) into positions + negate flag
.container_token_to_positions <- function(tok, nm, n, env, strict) {
    # Return list(pos = integer(), negate = FALSE)
    res <- list(pos = integer(0L), negate = FALSE)

    if (is.null(tok)) return(res)

    # list(...) inside the index should behave like comma-sugar
    if (is.call(tok) && identical(tok[[1L]], as.name("list"))) {
        args <- as.list(tok)[-1L]
        out <- lapply(
            args,
            FUN = .container_token_to_positions,
            nm = nm, n = n, env = env, strict = strict
        )
        # concatenate respecting negation rules later
        return(list(pos = out, negate = "LIST"))
    }

    # unary minus: -(a:b), -1, -c(1, 2), -c("a", "b")
    if (is.call(tok) && identical(tok[[1L]], as.name("-"))) {
        inner <- .container_token_to_positions(tok[[2L]], nm, n, env, strict)
        # If LIST, flatten first
        if (identical(inner$negate, "LIST")) {
            inner_flat <- .container_flatten_list_nodes(inner$pos, nm, n, env, strict)
            inner <- list(pos = inner_flat$pos, negate = FALSE)
        }
        inner$negate <- !isTRUE(inner$negate)
        return(inner)
    }

    # range operator :
    if (is.call(tok) && identical(tok[[1L]], as.name(":"))) {
        lhs <- tok[[2L]]; rhs <- tok[[3L]]
        L <- .container_endpoint_pos(lhs, nm, n, env, strict)
        R <- .container_endpoint_pos(rhs, nm, n, env, strict)
        if (anyNA(c(L, R))) {
        if (strict) stop("Range endpoints could not be resolved.", call. = FALSE)
        return(res)
        }
        rng <- if (L <= R) seq.int(L, R) else seq.int(L, R, by = -1L)
        res$pos <- as.integer(rng)
        return(res)
    }

    # bare name -> treat as container name if present, otherwise evaluate
    if (is.symbol(tok)) {
        key <- as.character(tok)
        if (key %in% nm) {
        res$pos <- match(key, nm)
        return(res)
        }
        # allow TRUE/FALSE as symbols to fall through to eval
    }

    # c(...) : evaluate normally; but we still want to support mixed types
    if (is.call(tok) && identical(tok[[1L]], as.name("c"))) {
        val <- try(suppressWarnings(eval(tok, env)), silent = TRUE)
        return(.container_value_to_positions(val, nm, n, strict))
    }

    # default: evaluate the token and map to positions
    val <- try(suppressWarnings(eval(tok, env)), silent = TRUE)
    .container_value_to_positions(val, nm, n, strict)
}

# Convert an already-evaluated value (numeric/character/logical) to positions
.container_value_to_positions <- function(val, nm, n, strict) {
  res <- list(pos = integer(0L), negate = FALSE)
  if (is.null(val)) return(res)

  if (is.logical(val)) {
    L <- length(val)
    if (L == 0L) return(res)
    if ((n %% L) != 0L)
      warning(sprintf("Logical index of length %d is not a multiple of %d; recycling.", L, n), call. = FALSE)
    idx <- rep_len(val, n)
    if (anyNA(idx)) warning("Logical index contains NA; treating NA as FALSE.", call. = FALSE)
    idx[is.na(idx)] <- FALSE
    res$pos <- which(idx)
    return(res)
  }

  if (is.numeric(val)) {
    v <- as.integer(val)
    v <- v[v != 0L]  # ignore zeros
    has_pos <- any(v > 0L, na.rm = TRUE)
    has_neg <- any(v < 0L, na.rm = TRUE)
    if (has_pos && has_neg)
      stop("Cannot mix positive and negative indices.", call. = FALSE)
    if (has_neg) return(list(pos = abs(v), negate = TRUE))
    res$pos <- v
    return(res)
  }

  if (is.character(val)) {
    pos <- match(val, nm, nomatch = NA_integer_)
    if (anyNA(pos)) {
      missing <- unique(val[is.na(pos)])
      if (strict)
        stop(sprintf("Unknown names in index: %s", paste0(dQuote(missing), collapse = ", ")), call. = FALSE)
      pos <- pos[!is.na(pos)]
    }
    res$pos <- as.integer(pos)
    return(res)
  }

  if (is.list(val)) {
    parts <- lapply(val, .container_value_to_positions, nm = nm, n = n, strict = strict)
    return(list(pos = parts, negate = "LIST"))
  }

  if (strict) stop(sprintf("Unsupported index type: %s", class(val)[1]), call. = FALSE)
  res
}

# Flatten a list of node results produced by list(...) or commas
.container_flatten_list_nodes <- function(nodes, nm, n, env, strict) {
  # nodes is a list of lists {pos, negate} possibly with nested LIST markers
  out_pos <- list()
  out_neg <- logical()
  for (node in nodes) {
    if (identical(node$negate, "LIST")) {
      inner <- .container_flatten_list_nodes(node$pos, nm, n, env, strict)
      out_pos <- c(out_pos, list(list(pos = inner$pos, negate = FALSE)))
      out_neg <- c(out_neg, FALSE)
    } else {
      out_pos <- c(out_pos, list(node))
      out_neg <- c(out_neg, isTRUE(node$negate))
    }
  }
  if (length(out_pos) == 0L) return(list(pos = integer(0L), any_neg = FALSE, all_neg = FALSE))
  list(
    pos     = unlist(lapply(out_pos, `[[`, "pos"), use.names = FALSE),
    any_neg = any(out_neg),
    all_neg = all(out_neg)
  )
}

# Top-level resolver: returns final positive positions (1-based, duplicates allowed)
.container_resolve_indices <- function(x, i_expr, dots_exprs, env, strict) {
  n  <- length(x)
  nm <- names(x) %||% rep.int("", n)

  # Build token list: i + ...
  toks <- c(if (!is.null(i_expr)) list(i_expr) else list(), dots_exprs)

  # list(...) in i -> treat as comma-sugar
  if (length(toks) == 1L && .is_call(toks[[1L]], "list")) {
    toks <- as.list(toks[[1L]])[-1L]
  }

  # if all tokens evaluate to logical, concatenate first
  all_logical <- length(toks) > 1L && all(vapply(toks, function(e) {
    v <- try(suppressWarnings(eval(e, env)), silent = TRUE)
    is.logical(v)
  }, logical(1)))

  if (all_logical) {
    v <- unlist(lapply(toks, function(e) eval(e, env)), use.names = FALSE)
    nodes <- list(.container_value_to_positions(v, nm, n, strict))
  } else {
    nodes <- lapply(toks, .container_token_to_positions,
                    nm = nm, n = n, env = env, strict = strict)
  }

  # Flatten any LIST markers (keep your existing block)
  if (any(vapply(nodes, function(z) identical(z$negate, "LIST"), logical(1)))) {
    nodes <- list(list(
      pos    = .container_flatten_list_nodes(
                 lapply(nodes, function(z) if (identical(z$negate, "LIST")) z$pos else z),
                 nm, n, env, strict
               )$pos,
      negate = FALSE
    ))
  }

  vals      <- lapply(nodes, `[[`, "pos")
  negatives <- vapply(nodes, function(z) isTRUE(z$negate), logical(1))

if (any(negatives)) {
  if (!all(negatives)) stop("Cannot mix positive and negative indices.", call. = FALSE)

  drop_pos <- as.integer(unlist(vals, use.names = FALSE))

  # STRICT: complain about any out-of-bounds negatives
  if (strict && any(drop_pos < 1L | drop_pos > n, na.rm = TRUE)) {
    bad <- unique(drop_pos[drop_pos < 1L | drop_pos > n])
    stop(sprintf("Out-of-bounds negative indices: %s", paste(bad, collapse = ", ")), call. = FALSE)
  }

  # NON-STRICT: ignore OOB negatives entirely (base R semantics)
  drop_pos <- unique(drop_pos[drop_pos >= 1L & drop_pos <= n & !is.na(drop_pos)])

  sel <- setdiff(seq_len(n), drop_pos)
  return(as.integer(sel))
}

  sel <- as.integer(unlist(vals, use.names = FALSE))
  if (strict && any(sel < 1L | sel > n, na.rm = TRUE)) {
    bad <- unique(sel[sel < 1L | sel > n])
    stop(sprintf("Out-of-bounds indices: %s", paste(bad, collapse = ", ")), call. = FALSE)
  }
  sel <- sel[sel >= 1L & sel <= n & !is.na(sel)]
  sel
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
#' co = container(a = 1, b = 2, c = 3, d = 4)
#'
#' # Numeric index
#' co[c(1, 4)]                          # [a = 1, d = 4]
#' co[1, 4]                             # same
#'
#' # Boolean index
#' co[c(TRUE, FALSE, TRUE, FALSE)]      # [a = 1, c = 3]
#' co[TRUE, FALSE, TRUE, FALSE]         # same
#'
#' # Mixed numeric and character index
#' co[list(1, "d")]                     # [a = 1, d = 4]
#' co[1, "d"]
#'
#' # Partial Boolean index (recycled)
#' co[c(TRUE, FALSE)]                   # [a = 1, c = 3]
#' co[TRUE, FALSE]                      # same
#'
#' # Negative numeric index
#' co[-c(1:2)]                          # [c = 3, d = 4]
#' co[-1, -4]                           # [b = 2, c = 3]
#'
#' # Alpha-numeric range selection (non-standard evaluation)
#' co[a:b]                              # [a = 1, b = 2]
#' co[a:b, d:c]                         # [a = 1, b = 2, d = 4, c = 3]
#' co[1:c]                              # [a = 1, b = 2, c = 3]
#' co[d:2]                              # [d = 4, c = 3]
#'
#' # Negative indices
#' co[-(1:2)]                           # [c = 3, d = 4]
#' co[-1, -4]                           # [b = 2, d = 4]
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
`[.Container` <- function(
    x, i, ...,
    strict = getOption("container.strict_index", FALSE)
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

    pos <- .container_resolve_indices(
        x, i_expr, dots_exprs,
        env = parent.frame(),
        strict = strict
    )

    # ---- apply selection to your Container ----
    # Replace this with your class-specific subsetter:
    #   - Keep order and duplicates in 'pos'
    #   - Preserve names
    #   - Return a Container, not a base list
    out <- peek_at(x, pos)

    # Ensure class stays "Container" (if x[pos] drops it)
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
