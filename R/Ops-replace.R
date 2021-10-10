#' Replace Parts of a Container Object
#'
#' @description Replace parts of a `Container` object similar
#' to R's base replace operators on lists.
#' @name OpsReplace
#' @param x `Container` object in which to replace elements.
#' @param i,...  indices specifying elements to replace. Indices
#' are `numeric` or `character` vectors or a `list` containing both.
#' @param name `character` string (possibly backtick quoted)
#' @param value the replacing value of `ANY` type
#' @details
#'  `[<-` replaces multiple values. The indices can be `numeric` or
#' `character` or both. They can be passed as a `vector` or `list`. Values can
#' be added by 'replacing' at new indices, which only works for `character`
#' indices.
#'
#' `[[<-` replaces a single value at a given `numeric` or `character` index.
#' Instead of an index, it is also possible to replace certain elements by
#' passing the element in curly braces (see Examples), that is, the object is
#' searched for the element and then the element is replaced by the value.
#'
#' `$<-` replaces a single element at a given name.
NULL


#' @rdname OpsReplace
#' @examples
#' co = container(a = 1, b = "bar")
#' (co[1:2] <- 1:2)
#'
#' \dontrun{
#' co[3] <- 3 # index out of range
#' }
#' (co[list(1, "b")] <- 3:4)   # mixed numeric/character index
#'
#' @export
"[<-.Container" = function(x, i, value)
{
    ref_replace_at(x, i, value, .add = TRUE)
}

#' @name ContainerS3
#' @rdname ContainerS3
#' @examples
#' co = container(a = 1, b = "bar")
#' (co[1:2] <- 1:2)
#'
#' \dontrun{
#' co[3] <- 3 # index out of range
#' }
#' (co[list(1, "b")] <- 3:4)   # mixed numeric/character index
#'
NULL



#' @rdname OpsReplace
#' @examples
#' co = container(a = 1, b = 2)
#' co[[1]] <- 9
#' co[["b"]] <- 8
#' co[["x"]] <- 7
#' co$z <- 99
#' print(co)
#'
#' # Replace 8 by 0
#' co[[{8}]] <- 0
#' print(co)
#'
#' @export
"[[<-.Container" = function(x, i, value)
{
    if (length(i) != 1)
        stop("index must be of length 1", call. = FALSE)

    isub = substitute(i)
    char1 = as.character(isub)[1]

    if (grepl(char1, pattern = "{", fixed = TRUE))
        ref_replace(x, old = i, new = value)
    else
        ref_replace_at(x, i, value, .add = TRUE)
}

#' @name ContainerS3
#' @rdname ContainerS3
#' @examples
#' co = container(a = 1, b = 2)
#' co[[1]] <- 9
#' co[["b"]] <- 8
#' co[["x"]] <- 7
#' co$z <- 99
#' print(co)
#'
#' # Replace 8 by 0
#' co[[{8}]] <- 0
#' print(co)
#'
NULL


#' @rdname OpsReplace
#' @examples
#'
#' co = container(a = 1, b = "bar")
#' co$f <- 3
#' co$b <- 2
#' co
#'
#' @export
"$<-.Container" = function(x, name, value)
{
    ref_replace_at(x, name, value, .add = TRUE)
}

#' @name ContainerS3
#' @rdname ContainerS3
#' @examples
#'
#' co = container(a = 1, b = "bar")
#' co$f <- 3
#' co$b <- 2
#' co
#'
NULL

