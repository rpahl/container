#' @title Deque (double-ended queue) constructors 
#' @description Deques are a generalization of stacks and queues typically
#' with methods to add, remove and access elements at both sides of the
#' underlying data sequence. As such, \code{\link[container]{deque}} can also be
#' used to mimic both stacks and simple queues.
#' @details Inherits from \code{\link[container]{container}} and extends it by
#' \code{\link[container]{pop}} and \code{\link[container]{peek}} methods, element
#' \code{\link[container]{count}}ing, and \code{\link[container]{reverse}} and
#'  \code{\link[container]{rotate}} functionality.
#' @name dequeS3
#' @param x (vector or list) initial elements of the deque
#' @return \code{\link[container]{Deque}} object
#' @seealso \code{\link[container]{container}}, \code{\link[container]{Deque}}
#' @export deque as.deque is.deque
#' @examples
#' # addleft
#' d <- 2 + deque(1L)
#' values(d)                                          # 2 1
#' values(3:1 + deque(0L))                 # 3 2 1 0
#' 
#' # count
#' count(deque(c("Lisa", "Bob", "Bob")), "Bob")     # 2
#' 
#' # peek and pop
#' d <- deque(1:3)
#' peek(d)                # 3
#' pop(d)                 # 3
#' pop(d)                 # 2
#' pop(d)                 # 1
#' \dontrun{
#' d$pop()              # Error: pop at empty Deque
#' }
#' 
#' d <- deque(1:3)
#' print(d)
#' reverse(d)   # 3 2 1
#' print(d)
#' 
#' rotate(d)
#' values(d)                           # 1 3 2
#' values(rotate(d, -1))               # 3 2 1
#' values(rotate(d, 2))               # 2 1 3
NULL

#' @rdname dequeS3
#' @details \code{deque(x=list())}: create \code{\link[container]{Deque}} object
deque <- function(x=list()) Deque$new(x)

#' @rdname dequeS3
#' @details \code{as.deque(x)}: convert x to \code{Deque} object
as.deque <- function(x) Deque$new(x)

#' @rdname dequeS3
#' @details \code{is.deque(x)}: check for \code{Deque} class
is.deque <- function(x) inherits(x, "Deque")


#' @title Deque and Dict S3 member functions
#' @name DequeDictS3funcs
#' @export peek pop
NULL

#' @rdname DequeDictS3funcs
peek <- function(x, ...) UseMethod("peek")
#' @rdname DequeDictS3funcs
pop <- function(x, ...) UseMethod("pop")


#' @title Deque S3 member functions
#' @name DequeS3funcs
#' @param deq The deque object.
#' @param elem an element of the deque 
#' @export addleft count peekleft popleft reverse rotate
NULL

#' @rdname DequeS3funcs
addleft <- function(x, ...) UseMethod("addleft")

#' @rdname DequeS3funcs
count <- function(x, ...) UseMethod("count")

#' @rdname DequeS3funcs
peekleft <- function(x) UseMethod("peekleft")

#' @rdname DequeS3funcs
popleft <- function(x) UseMethod("popleft")

#' @rdname DequeS3funcs
reverse <- function(x) UseMethod("reverse")

#' @rdname DequeS3funcs
rotate <- function(x, ...) UseMethod("rotate")


#' @rdname DequeS3funcs
#' @details addleft(deq): Peek at last element on the right side without removing
#'  it.
addleft.Deque <- function(deq, elem) deq$addleft(elem)

#' @rdname DequeS3funcs
#' @details count(deq): Count number of \code{elem} occurences.
count.Deque <- function(deq, elem) deq$count(elem)

#' @rdname DequeS3funcs
#' @details peek(deq): Peek at last element on the right side without removing
#'  it.
peek.Deque <- function(deq) deq$peek()

#' @rdname DequeS3funcs
#' @details peekleft(deq): Peek at first element on the left side without removing
#'  it.
peekleft.Deque <- function(deq) deq$peekleft()

#' @rdname DequeS3funcs
#' @details pop(deq): remove and return last element from the right side of the
#'  \code{deque}.
pop.Deque <- function(deq) deq$pop()

#' @rdname DequeS3funcs
#' @details popleft(deq): Remove and return an element from the left side of
#'  the \code{Deque}.
popleft.Deque <- function(deq) deq$popleft()

#' @rdname DequeS3funcs
#' @details reverse(deq): Reverse all elements of the \code{deque} in-place.
reverse.Deque <- function(deq) deq$reverse()

#' @rdname DequeS3funcs
#' @param n (integer) the number of positions to rotate
#' @details rotate(deq): Rotate the \code{Deque} elements n positions to the
#'  right. If n is negative, rotate to the left.
rotate.Deque <- function(deq, n=1L) deq$rotate(n)


#' @title Binary deque operators
#' @name dequeS3binOp
#' @param x1 primitive or \code{\link[container]{Deque}} object
#' @param x2 primitive or \code{\link[container]{Deque}} object
#' @return \code{\link[container]{Deque}} object
NULL

#' @rdname dequeS3binOp 
#' @details \code{x + deq}:
#' @export
`+.Deque` <- function(x1, x2)
{
    if (is.deque(x1)) {
        x1$clone()$add(x2)
    } else {
        x2$clone()$addleft(x1)
    }
}

