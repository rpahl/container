# Most of the code below is borrowed from the 'sets' package and extended by
# some LABEL.x functions for Container and other classes.
LABELS <-
function(x, max_width = NULL, dots = "...", unique = FALSE, limit = NULL, ...)
{
    x <- as.list(x)
    l <- length(x)

    ## recycle max_width and dots as needed
    if (!is.null(max_width))
        max_width <- rep_len(max_width, length.out = l)
    dots <- rep_len(dots, length.out = l)

    ## check existing labels
    ret <- names(x)
    if (is.null(ret))
        ret <- rep.int("", l)

    ## create a label for components without given one
    empty <- is.na(ret) | (ret == "")
    if (any(empty))
        ret[empty] <- sapply(x[empty], LABEL, limit, ...)

    ## check maximum width (max_width == NULL => unbounded)
    if (!is.null(max_width)) {
        too_long <- nchar(ret, "width") > max_width
        if (any(too_long)) {
            ret[too_long] <- strtrim(ret[too_long], max_width[too_long])

            ## possibly add dots
            if (!is.null(dots))
                ret[too_long] <- paste0(ret[too_long], dots[too_long])
          }
    }

    if (unique)
      ret <- make.unique(ret)

    ret
}

#' @export
LABEL <- function(x, limit = NULL, ...) UseMethod("LABEL")

#' @export
LABEL.default <- function(x, limit = NULL, ...) {
    paste0("<<", class(x)[1L], ">>")
}

#' @export
LABEL.matrix <- function(x, limit = NULL, ...) {
    sprintf("<<%ix%i matrix>>", nrow(x), ncol(x))
}

#' @export
LABEL.numeric <-
function(x, limit = NULL, ...) {
    if (is.null(limit))
        limit <- 2L
    .format_or_class(x, limit, ...)
}

#' @export
LABEL.factor <- LABEL.numeric

#' @export
LABEL.integer <- LABEL.numeric

#' @export
LABEL.logical <- LABEL.numeric

#' @export
LABEL.character <-
function(x, limit = NULL, quote = TRUE, ...) {
    if (is.null(limit))
        limit <- 2L
    if (quote)
        x <- ifelse(is.na(x), x, paste0("\"", x, "\""))
    .format_or_class(x, limit, ...)
}

#' @export
LABEL.list <-
function(x, limit = NULL, ...) {
    if (is.null(limit))
        limit <- 1L
    .format_or_class(x, limit, ...)
}

#' @export
LABEL.set <-
function(x, limit = NULL, ...) {
    if (is.null(limit))
        limit <- 6L
    .format_or_class(x, limit, ...)
}

#' @export
LABEL.gset <- LABEL.set

#' @export
LABEL.cset <- LABEL.set

#' @export
LABEL.tuple <- LABEL.set

#' @export
LABEL.interval <- LABEL.set

.format_or_class <-
function(x, limit, ...)
{
    l <- length(x)
    if (l == 0)
        return(paste0(class(x)[1L], "()"))

    if (l >= limit)
        return(paste0("<<", class(x)[1L], "(", l, ")>>"))

    if (is.integer(x))
        format(ifelse(is.na(x), x, paste0(x, "L")), ...)
    else
        format(x, ...)
}


#' @export
LABEL.Container <- function(x, limit = NULL, ...)
{
    if (is.null(limit))
        limit <- 6L
    .format_or_class(x, limit, ...)
}

#' @export
LABEL.data.frame <- function(x, limit = NULL, ...)
{
    sprintf("<<%s(%ix%i)>>", data.class(x), nrow(x), ncol(x))
}

#' @export
LABEL.data.table <- function(x, limit = NULL, ...)
{
    sprintf("<<%ix%i data.frame>>", nrow(x), ncol(x))
}

#' @export
LABEL.dict.table <- function(x, limit = NULL, ...)
{
    sprintf("<<data.frame(%ix%i)>>", nrow(x), ncol(x))
}

