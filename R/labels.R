# The basic code idea below is borrowed from the 'sets' package with some
# minor adjustments and fixes and extended by some functions to handle
# Container and other classes.
LABELS <-
function(x, limit = NULL, ...)
{
    x <- as.list(x)
    len <- length(x)

    # Where names are defined, we use them as labels
    labels <- names(x)
    if (is.null(labels))
        labels <- rep.int("", len)

    noLabel <- is.na(labels) | (labels == "")
    if (any(noLabel))
        labels[noLabel] <- sapply(x[noLabel], LABEL, limit, ...)

    labels
}


#' @export
LABEL <- function(x, limit = NULL, ...) UseMethod("LABEL")


#' @export
LABEL.default <- function(x, limit = NULL, ...)
    paste0("<<", class(x)[1L], ">>")


#' @export
LABEL.matrix <- function(x, limit = NULL, ...)
    sprintf("<<%ix%i matrix>>", nrow(x), ncol(x))

#' @export
LABEL.numeric <-
function(x, limit = NULL, ...)
{
    if (is.null(limit))
        limit <- 2L

    s = .format_or_class(x, limit, ...)
    if (length(s) > 1)
        s = paste0("(", toString(s), ")")
    s
}

#' @export
LABEL.factor <- LABEL.numeric

#' @export
LABEL.integer <- LABEL.numeric

#' @export
LABEL.logical <- LABEL.numeric

#' @export
LABEL.character <-
function(x, limit = NULL, quote = TRUE, ...)
{
    if (is.null(limit))
        limit <- 2L

    if (quote)
        x <- ifelse(is.na(x), x, paste0("\"", x, "\""))

    s = .format_or_class(x, limit, ...)

    if (length(s) > 1)
        s = paste0("(", toString(s), ")")
    s

}

#' @export
LABEL.list <- function(x, limit = NULL, ...)
{
    if (is.null(limit))
        limit <- 0L

    s = .format_or_class(x, limit, ...)

    if (length(s) > 1)
        s = paste0("list(", toString(s), ")")

    if (length(s) == 1 && !startsWith(s, "list") && !startsWith(s, "<"))
        s = paste0("list(", toString(s), ")")

    s
}


#' @export
LABEL.Container <- function(x, limit = NULL, ...)
{
    if (is.null(limit))
        limit <- 5L
    .format_or_class(x, limit, ...)
}

#' @export
LABEL.data.frame <- function(x, limit = NULL, ...)
    sprintf("<<%s(%ix%i)>>", data.class(x), nrow(x), ncol(x))


#' @export
LABEL.data.table <- function(x, limit = NULL, ...)
    sprintf("<<%ix%i data.table>>", nrow(x), ncol(x))


#' @export
LABEL.dict.table <- function(x, limit = NULL, ...)
    sprintf("<<dict.table(%ix%i)>>", nrow(x), ncol(x))


.format_or_class <-
function(x, limit, ...)
{
    l <- length(x)
    if (l == 0)
        return(paste0(class(x)[1L], "()"))

    if (l > limit)
        return(paste0("<<", class(x)[1L], "(", l, ")>>"))

    if (is.integer(x))
        format(ifelse(is.na(x), x, paste0(x, "L")), ...)
    else
        format(x, ...)
}


