#' Object labels
#'
#' @references Credits for the code idea below belong to the authors of the
#' very well written `sets` package.
#' @seealso [sets::LABEL()]
#' @noRd
#' @export
get_label <- function(x, vec.len = NULL, ...)
    UseMethod("get_label")


#' @export
get_label.default <- function(x, ...)
    paste0("<<", data.class(x), ">>")


.get_atomic_label <-
function(x, vec.len = NULL, ...)
{
    if (is.null(vec.len))
        vec.len = utils::strOptions()$vec.len

    s = .format_or_class_label(x, vec.len, ...)

    if (length(s) > 1)
        s = paste0("(", paste(s, collapse = " "), ")")
    s
}

#' @export
get_label.numeric <- .get_atomic_label

#' @export
get_label.factor <- .get_atomic_label

#' @export
get_label.integer <- .get_atomic_label

#' @export
get_label.logical <- .get_atomic_label

#' @export
get_label.character <-
function(x, vec.len = NULL, ...)
{
    if (is.null(vec.len))
        vec.len = utils::strOptions()$vec.len

    x <- ifelse(is.na(x), x, paste0("\"", x, "\""))
    .get_atomic_label(x)
}

#' @export
get_label.list <- function(x, vec.len = NULL, ...)
{
    if (is.null(vec.len))
        vec.len = utils::strOptions()$vec.len

    .format_or_class_label(x, vec.len, ...)
}


#' @export
get_label.Container <- function(x, vec.len = NULL, ...)
{
    if (is.null(vec.len))
        vec.len = utils::strOptions()$vec.len

    .format_or_class_label(x, vec.len, ...)
}

#' @export
get_label.matrix <- function(x, ...)
    sprintf("<<%ix%i matrix>>", nrow(x), ncol(x))

#' @export
get_label.data.frame <- function(x, ...)
    sprintf("<<%s(%ix%i)>>", data.class(x), nrow(x), ncol(x))


#' @export
get_label.data.table <- function(x, ...)
    sprintf("<<%ix%i data.table>>", nrow(x), ncol(x))


#' @export
get_label.dict.table <- function(x, ...)
    sprintf("<<dict.table(%ix%i)>>", nrow(x), ncol(x))




.format_or_class_label <- function(x, vec.len = NULL, ...)
{
    if (is.null(vec.len))
        vec.len = utils::strOptions()$vec.len

    len <- length(x)
    if (len == 0)
        return(paste0(data.class(x), "()"))

    createClassLabel = len > vec.len
    if (createClassLabel)
        return(paste0("<<", data.class(x), "(", len, ")>>"))

    isUsingIdentical = TRUE
    if (is.integer(x) && isUsingIdentical)
        format(ifelse(is.na(x), x, paste0(x, "L")), ...)
    else
        format(x, ...)
}

