#' Deque Class
#'
#' @description Deques are a generalization of stacks and queues typically
#' with methods to add, delete and access elements at both sides of the
#' underlying data sequence. As such, the [Deque()] can also be used to mimic
#' both stacks and queues.
#' @details This class inherits from class [Container()] and extends it by
#' `pop` and `peek` methods, and reverse and rotate functionality.
#' @importFrom R6 R6Class
#' @seealso [Container()], [deque()]
#' @export
Deque <- R6::R6Class("Deque",
    inherit = Container,
    public = list(
        #' @description Add element to left side of the `Deque`.
        #' @param value value of `ANY` type to be added to the `Deque`.
        #' @param name `character` optional name attribute of the value.
        #' @return the `Deque` object.
        addleft = function(value, name = NULL) {

            elem = list(value)
            names(elem) = name

            private$elems <- c(elem, private$elems)
            self
        },

        #' @description Peek at last element of the `Deque`.
        #' @param default returned default value if `Deque` is empty.
        #' @return element 'peeked' on the right
        peek = function(default = NULL) {
            super$peek_at2(self$length(), default)
        },

        #' @description Peek at first element of the `Deque`.
        #' @param default returned default value if `Deque` is empty.
        #' @return element 'peeked' on the left
        peekleft = function(default = NULL) {
            super$peek_at2(1, default)
        },

        #' @description
        #' Delete and return element from the left side of the [Deque()].
        #' @return element 'popped' from the left side of the [Deque()]
        popleft = function() {
            if (self$is_empty())
                stop("popleft at empty ", data.class(self), call. = FALSE)

            first <- self$peekleft()
            self$delete(first)
            first
        },

        #' @description Reverse all elements of the [Deque()] in-place.
        #' @return the `Deque()` object.
        rev = function() {
            len <- self$length()
            private$elems <- rev(private$elems)
            self
        },

        #' @description Rotate all elements `n` steps to the right. If n is
        #' negative, rotate to the left.
        #' @param n `integer` number of steps to rotate
        #' @return returns the `Deque()` object.
        rotate = function(n = 1L) {
            if (self$is_empty() || n == 0)
                return(self)

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

