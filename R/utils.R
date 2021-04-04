is_string = function(x)
{
    if (is.null(x))
        return(FALSE)

    if (length(x) != 1)
        return(FALSE)

    isTRUE(is.character(x) && !is.na(x))
}

is_nonempty_string = function(x)
{
    isTRUE(is_string(x) && nchar(x) > 0)
}

