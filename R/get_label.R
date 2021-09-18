#' Object labels
#'
#' @param x `ANY` R object
#' @param vec.len length of vector that is still printed
#' @param ... arguments passed to and from methods
#' @references Credits for the code idea below belong to the authors of the
#' very well written `sets` package.
#' @seealso [sets::LABEL()]
#' @export
get_label <-
function(x, vec.len = 4L, ...)
    UseMethod("get_label")


#' @export
get_label.default <-
function(x, ...) {
    if (is.null(x))
        return("NULL")

    s = class(x)[1L]

    if (!is.null(dim(x)))
        s = paste0(s, "(", paste(dim(x), collapse = "x"), ")")

    #if (is.object(x))
    paste0("<<", s, ">>")
}


.get_atomic_label <-
function(x, vec.len = 4L, useDots = TRUE, ...)
{
    s = .format_or_class_label(x, vec.len = ifelse(useDots, Inf, vec.len), ...)
    s = trimws(s)

    if (length(s) <= 1)
        return(s)

    if (length(s) > vec.len)
        s = s[1:vec.len]

    dots_or_empty = if(useDots && length(x) > vec.len) " ..." else ""

    paste0("(", paste(s, collapse = " "), dots_or_empty, ")")
}


#' @export
get_label.numeric <-
    .get_atomic_label

#' @export
get_label.factor <-
    .get_atomic_label

#' @export
get_label.integer <-
    .get_atomic_label

#' @export
get_label.logical <-
function(x, vec.len = 4L, ...)
{
    .get_atomic_label(x, vec.len = max(1L, vec.len / 2), ...)
}


#' @export
get_label.character <-
function(x, ...) {
    if (length(x))
        x <- ifelse(is.na(x), x, paste0("\"", x, "\""))

    .get_atomic_label(x, ...)
}


#' @export
get_label.list <-
function(x, ...) {
    .format_or_class_label(x, ...)
}


#' @export
get_label.Container <-
function(x, ...) {
    .format_or_class_label(x, ...)
}


#' @export
get_label.matrix <-
function(x, ...)
    get_label.default(x, ...)




.format_or_class_label <-
function(x, vec.len = 4L, markInteger = TRUE, ...)
{
    len <- length(x)
    if (len == 0)
        return(paste0(class(x)[1L], "()"))

    createClassLabel = len > vec.len
    if (createClassLabel)
        return(paste0("<<", class(x)[1L], "(", len, ")>>"))

    if (is.integer(x) && markInteger)
        format(ifelse(is.na(x), x, paste0(x, "L")), ...)
    else
        format(x, ...)
}

