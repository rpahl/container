
#' @export
Ops.Set <- function(e1, e2)
{
    s1 = sets::as.set(unpack(e1))
    s2 = sets::as.set(unpack(e2))
    result = get(.Generic)(s1, s2)

    if (is.logical(result))
        return(result)

    as.set(as.list(result))
}

