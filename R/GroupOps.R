#' @export
Ops.Set <- function(e1, e2)
{
    s1 = sets::set(unpack(e1))
    s2 = sets::set(unpack(e2))
    result = get(.Generic)(s1, s2)

    if (is.logical(result))
        return(result)
}

