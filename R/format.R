.format_values <-
function(x, left = "(", right = ")", ...)
{
    x.names <- names(x)
    name_seps <- rep.int("", length(x))

    if (!is.null(x.names))
        name_seps[x.names != ""] <- " = "

    labels = sapply(x, .get_label, ...)

    obj_str = paste(x.names, name_seps, labels, sep = "", collapse = ", ")
    paste0(left, obj_str, right)
}


format.Container <-
function(x, ...) {
    .format_values(x$values(), left = "[", right = "]", ...)
}

format.Deque <-
function(x, ...) {
    .format_values(x$values(), left = "|", right = "|", ...)
}

format.Dict <-
function(x, ...) {
    .format_values(x$values(), left = "{", right = "}", ...)
}

format.Set <-
function(x, ...) {
    .format_values(x$values(), left = "{", right = "}", ...)
}

format.OrderedSet <-
function(x, ...) {
    .format_values(x$values(), left = "{", right = "}", ...)
}


format.list <-
function(x, ...) {
    .format_values(x, left = "list(", right = ")", ...)
}

