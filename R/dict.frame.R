#' A Dict.frame class
#'
#' @description The `Dict.frame` is a mix of a dictionary and a `data.frame`.
#' @author Roman Pahl
#' @docType class
#' @importFrom R6 R6Class
#' @seealso [dict()], [base::data.frame()]
#' @section Inherited methods:
#' Inherits all methods from [dict()] but overrides the internal initialize
#' function and the following member functions:
#' * [add(key, value)]: if `key` not yet in `Dict.frame`, insert `value` at `key`,
#'   otherwise signal an error.
#' * [discard(key)]: if `key` in `Dict.frame`, remove it.
#' * [has(key)]: TRUE if `key` in `Dict.frame` else FALSE.
#' * [remove(key)]: if `key` in Dict.frame, remove it, otherwise raise an error.
#'
#' @section R6 constructor:
#' `Dict.frame$new(x = list())`
#'
#' @section Dict.frame methods:
#'  * [get(key)]: if `key` in `Dict.frame`, return value, else throw key-error.
#'  * [keys()]: return a character vector of all keys.
#'  * [peek(key, default=NULL)]: return the value for `key` if `key` is in the
#'    `Dict.frame`, else `default`.
#'  * [pop(key)]: if `key` in `Dict.frame`, return its value and discard it
#'    from the `Dict.frame`.
#'  * popitem: remove and return an arbitrary column from the Dict.frame. This
#'    function can be used to destructively iterate over a `Dict.frame`.
#'  * [set(key, value, add=FALSE)]: like `add` but overwrites value if `key` is
#'    already in the `Dict.frame`. If `key` not in `Dict`, an error is thrown
#'    unless `add` was set to `TRUE`.
#'  * [sort(decr = FALSE)]: sort columns in dict.frame according to column names.
#'  * [update(other = Dict.frame$new())]: add column(s) of `other` to the
#'    dict.frame if the key is not in the dictionary and update the column with
#'    the new column otherwise.
#' @examples
#' TODO:
#' @export
Dict.frame <- R6::R6Class("Dict.frame",
    inherit = Dict,
    public = list(
        initialize = function(x=list()) {},
        add = function(key, value) {},
        set = function(key, value, add=FALSE) {},
        update = function(other) {},

        nrow = function() private$nr,
        ncol = function() self$size(),
        print = function(...) {},
        rownames = function() private$row.names
    ),
    private = list(nr = integer(0),
                   row.names = character(0))
)

# Dict.frame method implementations
Dict.frame$set("public", "initialize", overwrite=TRUE,
    function(x = list()) {
        super$initialize(x)
        lens = lengths(self$values())
        hasFrame = length(unique(lens)) <= 1
        if (!hasFrame) {
            stop("All elements must have the same length.")
        }

        private$nr <- as.integer(lens[1])
        private$row.names <- if (is.data.frame(x)) {
            rownames(x)
        } else {
            as.character(seq_len(self$nr))
        }
        invisible(self)
    }
)

Dict.frame$set("public", "add", overwrite=TRUE,
    function(key, value) {
        if (self$has(key)) stop("key '", key, "' already in Dict.frame")
        self$set(key, value, add=TRUE)
    }
)

Dict.frame$set("public", "set", overwrite=TRUE,
    function(key, value, add=FALSE) {
        if (length(value) != self$nrow()) {
            stop("elements must be of length ", self$nrow())
        }
        if (!add) {
            if (!self$has(key)) stop("key '", key, "' not in Dict.frame")
        }
        private$elems[[key]] <- value
        invisible(self)
    }
)

Dict.frame$set("public", "update", overwrite=TRUE,
    # Add elements of other `Dict.frame` to the dict.frame if the key is not in
    # the dict.frame and update the column with the new elements otherwise.
    function(other = Dict.frame$new()) {
        if (!inherits(other, "Dict.frame")) stop("arg must be a Dict.frame")
        browser()
        if (self$nrow() != other$nrow()) {
            stop("mismatching number of rows")
        }
        for (key in other$keys()) {
            self$set(key, other$get(key), add=TRUE)
        }
        invisible(self)
    }
)


Dict.frame$set("public", "print", overwrite=TRUE,
    # If object can be printed as data.frame, do so, otherwise use default
    # print method
    function(...) {
        is_printable_as_data.frame = all(sapply(self$values(), is.atomic))
        if (is_printable_as_data.frame) {
            cat("<Dict.frame> of ", self$ncol(), " columns:\n", sep = "")
            print.data.frame(as.data.frame(self$values()), ...)
        } else {
            super$print(...)
        }
    }
)

Dict.frame$lock()

