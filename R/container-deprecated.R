#' Deprecated Functions in Package container
#'
#' @description These functions are provided for backwards-compatibility and
#' may be defunct as soon as the next release.
#' @param ... (arguments)
#' @seealso [base::Deprecated()]
#' @usage sortkey(...)  ### `Dict` keys are now always sorted
#' @usage bar(...)  ### -- use  system.time(...)  instead
#' @name container-deprecated
NULL


#' @title Sort Dict keys (deprecated)
#' @description Re-order elements according to key-order.This function
#' is deprecated as keys are now always sorted.
#' @param x any `R` object.
#' @param ... additional arguments to be passed to or from methods.
#' @param decr `logical` Should the sort be increasing or decreasing?
#' @name sortkey-deprecated
NULL

#' @rdname sortkey-deprecated
#' @export
sortkey <- function(x, decr = FALSE, ...) UseMethod("sortkey")

#' @rdname sortkey-deprecated
#' @return For `Dict` sorts the elements in place and inivisbly returns the
#' [Dict()] object.
#' @export
sortkey.Dict <- function(x, decr = FALSE) {
    .Deprecated(msg = "'sort' is deprecated - keys are now always sorted")
    invisible(x)
}

