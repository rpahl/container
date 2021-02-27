#' Deque (double-ended queue)
#'
#' @description Deques are a generalization of stacks and queues typically
#' with methods to add, delete and access elements at both sides of the
#' underlying data sequence. As such, the [Deque()] can also be used to mimic
#' both stacks and queues.
#' @details This class inherits from class [Container()] and extends it by
#' `pop` and `peek` methods, element counting, and reverse and rotate
#' functionality.
#' @importFrom R6 R6Class
#' @seealso [Container()], [deque()]
#' @export
Deque <- R6::R6Class("Deque",
    inherit = Container,
    public = list(
        #' @description Add element to left side of the `Deque`.
        #' @param x element to be added.
        #' @return invisibly returns the `Deque()` object.
        addleft = function(x) {
            private$elems <- c(list(x), private$elems)
            invisible(self)
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
            private$elems = utils::head(self$values(), n = -1)
            last
        },

        #' @description
        #' Delete and return element from the left side of the [Deque()].
        #' @return element 'popped' from the left side of the [Deque()]
        popleft = function() {
            if (self$empty()) stop("popleft at empty ", data.class(self))
            first <- self$peekleft()
            private$elems = utils::tail(self$values(), n = -1)
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
            if (self$empty() || n == 0)
                return(invisible(self))

            if (n > 0L) {
                last <- self$pop()
                self$addleft(last)
                self$rotate(n - 1)
            } else {
                first <- self$popleft()
                self$add(first)
                self$rotate(n + 1)
            }
        }
    ),
    lock_class = TRUE
)

