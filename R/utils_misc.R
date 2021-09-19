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


verify_names = function(x)
{
    if (!length(x) || !all(sapply(x, is_nonempty_string)))
        stop("all elements must be named", call. = FALSE)

    invisible(TRUE)
}


check_name_collision = function(x, y)
{
    common = intersect(x, y)

    if (!length(common))
        return(invisible(TRUE))

    if (length(common) == 1)
        stop("name ", paste0("'", common, "'"),
             " exists already", call. = FALSE)

    stop("names ", toString(paste0("'", common, "'")),
         " exist already", call. = FALSE)
}


