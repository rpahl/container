#' @title Deque (double-ended queue)
#' @description Deques are a generalization of stacks and queues typically
#' with methods to add, remove and access elements at both sides of the
#' underlying data sequence. As such, the \code{\link[container]{Deque}} can also be
#' used to mimic both stacks and queues.
#' @details Inherits from \code{\link[container]{Container}} and extends it by
#' \code{pop} and \code{peek} methods, element counting, and reverse and rotate
#' functionality.
#' @author Roman Pahl
#' @docType class
#' @importFrom R6 R6Class
#' @seealso \code{\link[container]{Container}}
#' @section Inherited methods:
#' Inherits all methods from \code{\link[container]{Container}} class.
#'
#' @section Deque methods:
#' \describe{
#'  \item{\code{addleft(elem)}}{Add \code{elem} to left side of the \code{Deque}.}
#'  \item{\code{count(elem)}}{Count number of \code{elem} occurences.}
#'  \item{\code{pop()}}{Remove and return element from the right side of the
#'  \code{Deque}.}
#'  \item{\code{popleft()}}{Remove and return an element from the left side of
#'  the \code{Deque}.}
#'  \item{\code{peek()}}{Peek at last element on the right side without removing it.}
#'  \item{\code{peekleft()}}{Peek at first element on the left side without
#'  removing it.}
#'  \item{\code{reverse()}}{Reverse all elements of the \code{Deque} in-place.}
#'  \item{\code{rotate(n=1)}}{Rotate the \code{Deque} elements n steps to the
#'      right. If n is negative, rotate to the left.}
#' }
#' @examples
#' # addleft
#' d <- Deque$new(1L)$addleft(2)
#' d$values()                                          # 2 1
#' Deque$new(0L)$addleft(3:1)$values()                 # 3 2 1 0
#'
#' # count
#' Deque$new(c("Lisa", "Bob", "Bob"))$count("Bob")     # 2
#'
#' # peek and pop
#' d <- Deque$new(1:3)
#' d$peek()                # 3
#' d$pop()                 # 3
#' d$pop()                 # 2
#' d$pop()                 # 1
#' #' \dontrun{
#' #' d$pop()              # Error: pop at empty Deque
#' #' }
#'
#' Deque$new(1:3)$reverse()$values()   # 3 2 1
#'
#' Deque$new(1:3)$rotate()$values()    # 3 1 2
#' Deque$new(1:3)$rotate(2)$values()   # 2 3 1
#' Deque$new(1:3)$rotate(-1)$values()  # 2 3 1
#' @export
Deque <- R6::R6Class("Deque",
    inherit = Container,
    public = list(
        addleft = function(elem) {},
        count = function(elem) {},
        peek = function() {},
        peekleft = function() {},
        pop = function() {},
        popleft = function() {},
        reverse = function() {},
        rotate = function(n=1L) {}
    )
)


# Deque method implementations
Deque$set("public", "addleft", overwrite=TRUE,
    function(elem) {
        if (self$type() == "list") {
            private$elems <- c(list(elem), private$elems)
        } else {
            v <- Reduce(f=c, x=elem, init=private$elems, right=TRUE)
            private$elems <- as.vector(v, mode=self$type())
        }
        invisible(self)
    }
)

Deque$set("public", "count", overwrite=TRUE,
    function(elem) {
        comp <- function(x) isTRUE(all.equal(x, elem))
        sum(sapply(private$elems, FUN=comp))
    }
)

Deque$set("public", "peek", overwrite=TRUE,
    function() {
        len <- self$size()
        last <- if (len > 0) private$elems[[len]] else NULL
        last
    }
)

Deque$set("public", "peekleft", overwrite=TRUE,
    function() {
        len <- self$size()
        first <- if (len > 0) private$elems[[1]] else NULL
        first
    }
)

Deque$set("public", "pop", overwrite=TRUE,
    function() {
        if (self$empty()) stop("pop at empty ", data.class(self))
        last <- self$peek()
        self$remove(last, right=TRUE)
        last
    }
)

Deque$set("public", "popleft", overwrite=TRUE,
    function() {
        if (self$empty()) stop("popleft at empty ", data.class(self))
        first <- self$peekleft()
        self$remove(first)
        first
    }
)

Deque$set("public", "reverse", overwrite=TRUE,
    function() {
        len <- self$size()
        private$elems <- rev(private$elems)
        invisible(self)
    }
)

Deque$set("public", "rotate", overwrite=TRUE,
    function(n=1L) {
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
)
Deque$lock()

