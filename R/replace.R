#' Strict element replacement
#'
#' @description Try to find and replace elements and signal an error if not
#' found, unless it is stated to explicitly add the element (see option `add`).
#' @param .x any `R` object.
#' @param ... additional arguments to be passed to or from methods.
#' @param add `logical` if `FALSE` (default) and element was not found,
#' an error is given. In contrast, if set to `TRUE` the new element is added
#' regardless of whether it is used as a replacement for an existing element or
#' just added as a new element.
#' @details `replace` uses copy semantics while `ref_replace` works by reference.
#' @export
replace <- function(.x, ...) UseMethod("replace")

#' @rdname replace
#' @export
ref_replace <- function(.x, ...) UseMethod("ref_replace")

#' @export
replace.default <- function(.x, ...)
{
    base::replace(.x, ...)
}


#' @rdname replace
#' @param old old element to be found and replaced.
#' @param new the new element replacing the old one.
#' @return For `Container`, an object of class `Container` (or one of the
#' respective derived classes).
#' @examples
#'
#' co = container("x", 9)
#' replace(co, 9, 0)
#' replace(co, "x", 0)
#' \dontrun{
#' replace(co, "z", 0)              # old element ("z") is not in Container}
#' replace(co, "z", 0, add = TRUE)  # just add the zero without replacement
#'
#' @export
replace.Container <- function(.x, old, new, add = FALSE)
{
    ref_replace(.x$clone(deep = TRUE), old, new, add)
}

#' @name replace.Container
#' @rdname ContainerS3
#' @param old old element to be found and replaced.
#' @param new the new element replacing the old one.
#' @param add `logical` if FALSE (default) and `old` element was not found,
#' an error is given. In contrast, if set to `TRUE` the new element is added
#' regardless of whether it is used as a replacement for an existing or
#' just as a new element, respectively.
#' @usage
#' replace(.x, old, new, add = FALSE)
#' ref_replace(.x, old, new, add = FALSE)
#' @details
#' * `replace(.x, old, new, add = FALSE)` and
#'   `ref_replace(.x, old, new, add = FALSE)` try to find element `old` and
#' replace it with element `new`. If `old` does not exist, an error is raised,
#' unless `add` was set to `TRUE`.
#' @examples
#'
#' co = container("x", 9)
#' replace(co, 9, 0)
#' replace(co, "x", 0)
#' \dontrun{
#' replace(co, "z", 0)              # old element ("z") is not in Container}
#' replace(co, "z", 0, add = TRUE)  # ok, adds the element
NULL

#' @rdname replace
#' @export
ref_replace.Container <- function(.x, old, new, add = FALSE)
{
    .x$replace(old, new, add = add)
}



#' @rdname replace
#' @return For `Dict` an object of class `Dict`.
#' @examples
#'
#' d = dict(a = 1, b = "z")
#' replace(d, 1, 1:5)
#' replace(d, "z", "a")
#'
#' \dontrun{
#' replace(d, "a", 2)              # old element ("a") is not in Dict}
#'
#' @export
replace.Dict <- function(.x, old, new)
{
    ref_replace(.x$clone(deep = TRUE), old, new)
}

#' @rdname replace
#' @export
ref_replace.Dict <- function(.x, old, new)
{
    .x$replace(old, new)
}

#' @name replace.Dict
#' @param old old element to be found and replaced.
#' @param new the new element replacing the old one.
#' @rdname DictS3
#' @usage
#' replace(.x, old, new)
#' ref_replace(.x, old, new)
#' @details
#' * `replace(.x, old, new)` and `ref_replace(.x, old)` try to find element `old`
#'  and replace it with element `new`. If `old` does not exist, an error is
#'  raised.
#' @examples
#'
#' d = dict(a = 1, b = "z")
#' replace(d, 1, 1:5)
#' replace(d, "z", "a")
#'
#' \dontrun{
#' replace(d, "a", 2)              # old element ("a") is not in Dict}
#'
NULL

