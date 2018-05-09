#' @title A Dict class
#' @description The \code{Dict} resembles Python's dict type, and is implemented
#' as a specialized associative (or mapping) \code{\link[container]{Container}} thus
#' sharing all \code{\link[container]{Container}} methods with some of them being
#' overriden to account for the associative key-value pair semantic.
#' @author Roman Pahl
#' @docType class
#' @importFrom R6 R6Class
#' @seealso \code{\link[container]{Container}}
#' @section Inherited methods:
#' Inherits all methods from \code{\link[container]{Container}} but overrides the
#' internal initialize function and the following member functions:
#' \describe{
#'  \item{\code{add(key, value)}}{If \code{key} not yet in \code{Dict}, set
#'      \code{key} to \code{elem}, otherwise raise an error.}
#'  \item{\code{discard(key)}}{If \code{key} in \code{Dict}, remove it.}
#'  \item{\code{get(key)}}{If \code{key} in \code{Dict}, return value, else
#'  throw key-error.}
#'  \item{\code{has(key)}}{TRUE if \code{key} in \code{Dict} else FALSE.}
#'  \item{\code{remove(key)}}{If \code{key} in \code{Dict}, remove it, otherwise
#'      raise an error.}
#' }
#'
#' @section Dict methods:
#' \describe{
#'  \item{\code{keys()}}{Return a character vector of all keys.}
#'  \item{\code{pop(key)}}{If \code{key} in \code{Dict}, return a copy of its
#'      value and discard it afterwards.}
#'  \item{\code{popitem()}}{Remove and return an arbitrary (key, value) pair
#'  from the dictionary. \code{popitem()} is useful to destructively iterate
#'  over a \code{Dict}, as often used in set algorithms.}
#'  \item{\code{peek(key, default=NULL)}}{Return the value for \code{key} if
#'      \code{key} is in the \code{Dict}, else \code{default}.}
#'  \item{\code{set(key, value)}}{Like \code{add} but overwrites value if
#'      \code{key} is already in the \code{Dict}.}
#' }
#' @examples
#' ages <- Dict$new(c(Peter=24, Lisa=23, Bob=32))
#' ages$has("Peter")   # TRUE
#' ages$peek("Lisa")   # 23
#' ages$peek("Mike")   # NULL
#' ages$add("Mike", 18)
#' ages$peek("Mike")   # 18
#' ages$keys()
#'
#' \dontrun{
#' Dict$new(list(Peter=20))$add("Peter", 22)         # key already in Dict
#' Dict$new(c(Peter=24, Lisa=23, Bob=32, Peter=20))  # Error: duplicated keys
#' }
#' @export
Dict <- R6::R6Class("Dict",
    inherit = Container,
    public = list(
        initialize = function(x=list()) {
            name_len <- sapply(names(x), nchar)
            if (length(x) != length(name_len) || any(name_len == 0)) {
                stop("all elems must be named")
            }
            super$initialize(x)
            if (any(duplicated(self$keys()))) stop("duplicated keys")
            invisible(self)
        },
        add = function(key, value) {
            if (!is.character(key)) stop("key must be character")
            if (length(key) != 1) stop("key must be single character string")
            if (nchar(key) == 0) stop("zero-length key")
            if (self$has(key)) stop("key '", key, "' already in Dict")
            self$set(key, value)
        },
        discard = function(key) {
            if (self$has(key)) {
                pos <- match(key, self$keys())
                private$elems <- private$elems[-pos]
            }
            invisible(self)
        },
        get = function(key) {
            if (self$has(key)) {
                self$peek(key)
            } else {
                stop("key '", key, "' not in Dict")
            }
        },
        has = function(key) key %in% self$keys(),
        keys = function() as.character(names(private$elems)),
        peek = function(key, default=NULL) {
            if (self$has(key)) private$elems[[key]] else default
        },
        pop = function(key) {
            elem <- self$peek(key)
            self$remove(key)
            elem
        },
        popitem = function() {
            if (self$empty()) {
                stop("pop at empty ", data.class(self))
            } else {
                key <- sample(self$keys(), 1)
                key_value_pair <- private$elems[key]
                self$remove(key)
                return(key_value_pair)
            }
        },
        remove = function(key) {
            if (!self$has(key)) stop("key '", key, "' not in Dict")
            self$discard(key)
        },
        set = function(key, value) {
            private$elems[[key]] <- value
            invisible(self)
        }
    ),
    lock_class=TRUE
)

