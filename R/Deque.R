#' A Deque (double-ended queue) class
#'
#' @description Deques are a generalization of stacks and queues typically
#' with methods to add, delete and access elements at both sides of the
#' underlying data sequence. As such, the [Deque()] can also be used to mimic
#' both stacks and queues.
#' @details This class inherits from class [Container()] and extends it by
#' `pop` and `peek` methods, element counting, and reverse and rotate
#' functionality.
#' @author Roman Pahl
#' @importFrom R6 R6Class
#' @seealso [Container()], [deque()]
#' @export
Deque <- R6::R6Class("Deque",
    inherit = Container,
    public = list(
        #' @description Add elem to left side of the `Deque`.
        #' @param elem element to be added.
        #' @return invisibly returns the `Deque()` object.
        addleft = function(elem) {
            type <- mode(self$values())
            if (type == "list") {
                elem <- list(elem)
            } else {
                if (type != mode(elem)) {
                    stop("type mismatch: expected '", type,
                         "' but got '", mode(elem), "'")
                }
            }
            private$elems <- c(elem, private$elems)
            invisible(self)
        },

        #' @description Count number of elem occurences.
        #' @param elem element to be counted.
        #' @return `integer` number of `elem` occurences in the [Deque()]
        count = function(elem) {
            comp <- function(x) isTRUE(all.equal(x, elem))
            sum(sapply(private$elems, FUN = comp))
        },

        #' @description Peek at last element on the right without removing it.
        #' @return element 'peeked' on the right
        peek = function() {
            len <- self$length()
            last <- if (len > 0) .subset2(private$elems, len) else NULL
            last
        },

        #' @description Peek at first element on the left without removing it.
        #' @return element 'peeked' on the left
        peekleft = function() {
            len <- self$length()
            first <- if (len > 0) .subset2(private$elems, 1) else NULL
            first
        },

        #' @description
        #' Delete and return element from the right side of the [Deque()].
        #' @return element 'popped' from the right side of the [Deque()]
        pop = function() {
            if (self$empty()) stop("pop at empty ", data.class(self))
            last <- self$peek()
            self$delete(last, right = TRUE)
            last
        },

        #' @description
        #' Delete and return element from the right side of the [Deque()].
        #' @return element 'popped' from the left side of the [Deque()]
        popleft = function() {
            if (self$empty()) stop("popleft at empty ", data.class(self))
            first <- self$peekleft()
            self$delete(first)
            first
        },

        #' @description Reverse all elements of the [Deque()] in-place.
        #' @return invisibly returns the `Deque()` object.
        rev = function() {
            len <- self$length()
            private$elems <- rev(private$elems)
            invisible(self)
        },

        #' @description Rotate all elements `n` steps to the right. If n is
        #' negative, rotate to the left.
        #' @param n `integer` number of steps to rotate
        #' @return invisibly returns the `Deque()` object.
        rotate = function(n = 1L) {
            # Rotate deque n steps to the right. If n is negative, rotate left.
            if (self$empty()) return(invisible(self))
            if (n >= 0L) {
                for (i in seq_len(n)) {
                    last <- self$pop()
                    self$addleft(last)
                }
            } else {
                for (i in seq_len(-n)) {
                    first <- self$popleft()
                    self$add(first)
                }
            }
            invisible(self)
        }
    ),
    lock_class = TRUE
)

