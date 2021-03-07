.create_object_string <- function(x, x.names, name_seps, ...)
{
    if (length(x) == 0) return("")

    paste(x.names, name_seps, LABELS(as.list(x), ...),
          sep = "", collapse = ", ")
}

.format_values <- function(x, left = "(", right = ")", ...)
{
    x.names <- names(x)
    names(x) <- NULL
    name_seps <- rep.int("", length(x))

    if (!is.null(x.names))
        name_seps[x.names != ""] <- " = "

    obj_str = .create_object_string(x, x.names, name_seps, ...)

    paste0(left, obj_str, right)
}


format.Container <- function(x, ...)
{
    .format_values(as.list(x), left = "[", right = "]", ...)
}

format.Deque <- function(x, ...)
{
    .format_values(as.list(x), left = "|", right = "|", ...)
}

format.Dict <- function(x, ...)
{
    .format_values(as.list(x), left = "{", right = "}", ...)
}

format.Set <- function(x, ...)
{
    .format_values(as.list(x), left = "{", right = "}", ...)
}

