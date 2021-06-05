.assert_index = function(x, index, ...)
{
    if (missing(index))
        stop("'index' is missing", call. = FALSE)

    if (length(index) != 1)
        stop("index must be of length 1", call. = FALSE)

    if (is.na(index))
        stop("index must not be 'NA'", call. = FALSE)

    switch(data.class(index),
           "character" = .assert_index.character(x, index),
           "numeric" = .assert_index.numeric(x, index),
           "integer" = .assert_index.numeric(x, index),
           .assert_index.default(x, index))
}


.assert_index.character = function(x, index)
{
    if (!(utils::hasName(x, index)))
        stop("index '", index, "' not found", call. = FALSE)

    invisible(TRUE)
}

.assert_index.numeric = function(x, index)
{
    if (isTRUE(index < 1))
        stop("index must be > 0", call. = FALSE)

    if (index > length(x))
        stop("index ", index, " exceeds length of ",
             data.class(x), ", which is ", length(x), call. = FALSE)

    invisible(TRUE)
}

.assert_index.default = function(x, index, ...)
{
    stop("invalid index type '", data.class(index), "'", call. = FALSE)
}


