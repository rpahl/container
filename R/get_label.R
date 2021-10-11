# Credits for the basic code idea belong to the authors of the `sets` package.

.get_label.default = function(x, ...)
{
    if (is.null(x))
        return("NULL")

    s = class(x)[1L]

    if (!is.null(dim(x)))
        s = paste0(s, "(", paste(dim(x), collapse = "x"), ")")

    paste0("<<", s, ">>")
}


.get_atomic_label = function(x, vec.len = 4L, useDots = TRUE, ...)
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


.get_label.logical = function(x, vec.len = 4L, ...)
    .get_atomic_label(x, vec.len = max(1L, vec.len / 2), ...)


.get_label.character = function(x, ...)
{
    if (length(x))
        x <- ifelse(is.na(x), x, paste0("\"", x, "\""))

    .get_atomic_label(x, ...)
}


.format_or_class_label = function(x, vec.len = 4L, markInteger = TRUE, ...)
{
    len <- length(x)
    if (len == 0)
        return(paste0(class(x)[1L], "()"))

    doCreateClassLabel = len > vec.len
    if (doCreateClassLabel)
        return(paste0("<<", class(x)[1L], "(", len, ")>>"))

    if (is.integer(x) && markInteger)
        format(ifelse(is.na(x), x, paste0(x, "L")), ...)
    else
        format(x, ...)
}


setGeneric(".get_label", .get_label.default)

for (type in c("numeric", "factor", "integer"))
    setMethod(".get_label", signature(type), .get_atomic_label)

for (type in c("list", "Container", "Deque", "Dict", "Set"))
     setMethod(".get_label", signature(type), .format_or_class_label)

for (type in c("matrix", "data.frame"))
    setMethod(".get_label", signature(type), .get_label.default)


setMethod(".get_label", signature("logical"), .get_label.logical)
setMethod(".get_label", signature("character"), .get_label.character)

