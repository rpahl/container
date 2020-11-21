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

        #' @description Dimensions of the `Dict.frame` object
        #' @return `integer` number of columns
        dim = function() c(self$nrow(), self$ncol()),

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
            if (len < 2) stop("len must be > 1")
            is_printable_as_data.frame = all(sapply(self$values(), is.atomic))
            if (is_printable_as_data.frame) {
                title = paste("<Dict.frame> with",
                               self$ncol(), "columns and",
                               self$nrow(), "rows")
                cat(title, "\n")
                if (self$nrow() == 0) return()

                types = sapply(self$apply(typeof), abbreviate, minlength= 3)
                types = paste0("<", types, ">")

                rows = seq_len(self$nrow())
                half = min(length(rows), as.integer(len / 2))
                top = head(rows, n = half)
                df = as.data.frame(Reduce(self$values(), f = cbind))
                if (nrow(df) > 0) {
                    rownames(df) = self$rownames()
                }
                colnames(df) = self$keys()
                df.top = df[top, , drop = FALSE]
                mid = NULL
                df.bottom = NULL
                bottom = setdiff(tail(rows, n = half), top)
                if (length(bottom)) {
                    if (bottom[1] - tail(top, 1) > 1)
                    mid = rep(".", ncol(df))
                    df.bottom = df[bottom, , drop = FALSE]
                }
                df.print = rbind(df.top, mid, df.bottom,
                                 stringsAsFactors = FALSE)
                hasSplit = !is.null(mid)
                if (nrow(df.print) > max(top) + 1) {
                    rownames(df.print)[nrow(df.top) + 1] <- "."
                }
                print(df.print, ...)
            } else {
                super$print(list.len = len, ...)
            }
        },

        #' @description combine with another [Dict.frame()] object. For this,
        #' columns must match.
        #' @return a copy of original `Dict.frame` with a copy of `other`
        #' attached at the bottom.
        rbind = function(other) {
            stopifnot(inherits(other, "Dict.frame"))
            stopifnot(identical(self$keys(), other$keys()))
            d = Dict.frame$new()
            for (key in self$keys()) {
                d$add(key, value = c(self$get(key), other$get(key)))
            }
            d
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

