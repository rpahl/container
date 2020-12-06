#' @noRd
add_class <- function(x, class, left = TRUE)
{
    stopifnot(is.character(class))
    stopifnot(is.logical(left))

    before = attr(x, "class")
    after = if (left) c(class, before) else c(before, class)

    attr(x, "class") <- after
    x
}

#' @noRd
assign_if <- function(x, value, pos)
{
    if (exists(x, pos, inherits = FALSE)) {
        assign(x, value, pos, inherits = FALSE)
    }
}

