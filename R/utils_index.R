.assert_index_arg = function(index)
{
    if (missing(index))
        stop("'index' is missing", call. = FALSE)

    if (length(index) != 1)
        stop("index must be of length 1", call. = FALSE)

    if (is.na(index))
        stop("index must not be 'NA'", call. = FALSE)
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

setGeneric(".assert_index",
           function(x, index) standardGeneric(".assert_index"))

setMethod(".assert_index",
          signature("ANY", "character"), .assert_index.character)

setMethod(".assert_index",
          signature("ANY", "numeric"), .assert_index.numeric)


assert_index = function(x, index)
{
    .assert_index_arg(index)
    .assert_index(x, index)
}


.has_index = function(x, index)
{
    stop("not supported for index of type '", data.class(index), "'")
}


.has_index.character = function(x, index)
{
    nzchar(index) && utils::hasName(x, index)
}

.has_index.numeric = function(x, index)
{
    isTRUE(index >= 1) && isTRUE(index <= length(x))
}

.has_index.integer = function(x, index)
{
    isTRUE(index >= 1) && isTRUE(index <= length(x))
}

setGeneric(".has_index", function(x, index) standardGeneric(".has_index"))

setMethod(".has_index",
          signature("ANY", "character"), .has_index.character)

setMethod(".has_index",
          signature("ANY", "numeric"), .has_index.numeric)

setMethod(".has_index",
          signature("ANY", "integer"), .has_index.integer)

has_index = function(x, index)
{
    .assert_index_arg(index)
    .has_index(x, index)
}

