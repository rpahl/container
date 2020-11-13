#' A Dict.frame class
#'
#' @description The [Dict.frame()] is a mix of a dictionary and a
#' `data.frame`. In particular, it is a dictionary thereby inheriting all
#' [Dict()] methods, but where each element has the same length. In contrast to
#' a [base::data.frame()], a [Dict.frame()] can contain arbitrary complex
#' objects.
#'
#' @author Roman Pahl
#' @seealso [Dict()], [dict.frame()], [base::data.frame()]
#' @export
Dict.frame <- R6::R6Class("Dict.frame",
    inherit = Dict,
    public = list(
        #' @description `Dict.frame` constructor
        #' @param x initial elements put into the `Dict`
        #' @return invisibly returns the `Dict.frame`
        initialize = function(x = list()) {
            super$initialize(x)
            lens = lengths(self$values())
            if (length(lens)) {
                hasFrame = length(unique(lens)) == 1
                if (!hasFrame) {
                    stop("All elements must have the same length.")
                }
                private$nr <- as.integer(lens[1])

                rn <- if (is.data.frame(x)) {
                    attr(x, "row.names")
                } else {
                    as.integer(seq_len(private$nr))
                }
                self$set_rownames(rn)
            }
            invisible(self)
        },

        #' @description Number of columns
        #' @return `integer` number of columns
        ncol = function() self$size(),

        #' @description Number of rows
        #' @return `integer` number of rows
        nrow = function() private$nr,

        #' @description If possible, object is printed as [base::data.frame()]
        #' with `len` rows, otherwise as structure using [utils::str()].
        #' @param len `integer` number of rows or elements shown
        #' @param ... other arguments passed to print method
        print = function(len = 10L, ...) {
            is_printable_as_data.frame = all(sapply(self$values(), is.atomic))
            if (is_printable_as_data.frame) {
                cat("<Dict.frame> with ", self$ncol(), " columns and ",
                    self$nrow(), " rows\n", sep = "")
                if (self$nrow() == 0) return()
                rows = seq_len(self$nrow())
                half = min(length(rows), as.integer(len / 2))
                top = head(rows, n = half)
                df = as.data.frame(Reduce(self$values(), f = cbind))
                rownames(df) = rownames(self)
                colnames(df) = self$keys()
                print.data.frame(df[top, , drop = FALSE], ...)
                bottom = setdiff(tail(rows, n = half), top)
                if (length(bottom)) {
                    if (bottom[1] - tail(top, 1) > 1)
                    cat("...\n")
                    print.data.frame(df[bottom, , drop = FALSE], ...)
                }
            } else {
                super$print(list.len = len, ...)
            }
        },

        #' @description Row names
        #' @return the row names of the [Dict.frame()]
        rownames = function() private$row.names,

        #' @description Overrides `value` at `key` if `key` is already in the
        #' `Dict.frame`. If `key` not in `Dict.frame`, an error is thrown
        #' unless `add` was set to `TRUE`.
        #' @param key `character` name of key.
        #' @param value the value to be set
        #' @param add `logical` if `TRUE` the value is set regardless whether
        #' `key` already exists in `Dict.frame`.
        #' @return invisibly returns the `Dict.frame` object
        set = function(key, value, add = FALSE) {
            isFirstValue = self$empty() && add
            if (isFirstValue) {
                private$nr = length(value)
                self$set_rownames(seq_along(value))
            }

            if (length(value) != self$nrow()) {
                stop("elements must be of length ", self$nrow())
            }
            super$set(key, value, add)
        },

        #' @description set row names
        #' @param x `numeric` or `character` vector of names
        #' @return invisibly returns the `Dict.frame` object
        set_rownames = function(x) {
            stopifnot(length(x) == self$nrow())
            private$row.names = x
            invisible(self)
        }
    ),
    private = list(nr = 0, row.names = character(0)),
    lock_class = TRUE
)

