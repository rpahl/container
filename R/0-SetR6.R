#' Set Class
#'
#' @description The [Set] is considered and implemented as a specialized
#' [Container], that is, elements are always unique in the [Container] and
#' it provides typical set operations such as `union` and `intersect`.
#' For the standard S3 interface, see [setnew()].
#' @seealso [Container], [set()]
#' @export
#' @examples
#' s1 = Set$new(1, 2)
#' s1
#' s1$add(1)
#' s1$add(3)
#' s2 = Set$new(3, 4, 5)
#' s1$union(s2)
#' s1
#'
#' s1 = Set$new(1, 2, 3)
#' s1$intersect(s2)
#' s1
#'
#' s1$diff(s2)
#' s1$diff(s1)
#' s1
Set <- R6::R6Class("Set",
    inherit = Container,
    public = list(
        #' @description `Set` constructor
        #' @param ... initial elements put into the `Set`
        #' @return returns the `Set` object
        initialize = function(...) {

            super$initialize()

            it = iter(list(...), .subset = .subset)
            while (has_next(it)) {
                value = get_next(it)
                self$add(value[[1]], name = names(value))
            }

            self
        },

        #' @description Add element
        #' @param value value of `ANY` type to be added to the `Set`.
        #' @param name `character` optional name attribute of the value.
        #' @return the `Set` object.
        add = function(value, name = NULL) {

            if (self$has(value))
                return(self)

            elem = list(value)
            names(elem) = name

            hash_value = private$.get_hash_value(value)
            private$elems[[hash_value]] = elem

            self
        },

        #' @description `Set` difference
        #' @param s `Set` object to 'subtract'
        #' @return the `Set` object updated as a result of the set difference
        #' between this and s.
        diff = function(s) {
            private$.verify_same_class(s)
            lapply(s$values(), self$discard)
            self
        },

        #' @description `Set` intersection
        #' @param s `Set` object to 'intersect'
        #' @return the `Set` object as a result of the intersection of this and s.
        intersect = function(s) {
            private$.verify_same_class(s)
            for (elem in self$values()) {
                if (!s$has(elem))
                    self$discard(elem)
            }
            self
        },

        #' @description `Set` union
        #' @param s `Set` object to be 'unified'
        #' @return the `Set` object as a result of the union of this and s.
        union = function(s) {
            private$.verify_same_class(s)

            it = s$iter()

            while (has_next(it)) {
                value = get_next(it)
                self$add(value[[1]], name = names(value))
            }

            self
        },

        #' @description `Set` equality
        #' @param s `Set` object to compare against
        #' @return `TRUE` if this is equal to `s`, otherwise `FALSE`
        is_equal = function(s) {
            private$.verify_same_class(s)
            self == s
        },

        #' @description `Set` proper subset
        #' @param s `Set` object to compare against
        #' @return `TRUE` if this is subset of `s`, otherwise `FALSE`
        is_subset = function(s) {
            private$.verify_same_class(s)
            self <= s
        },

        #' @description `Set` subset
        #' @param s `Set` object to compare against
        #' @return `TRUE` if this is proper subset of `s`, otherwise `FALSE`
        is_proper_subset = function(s) {
            private$.verify_same_class(s)
            self < s
        },

        #' @description Get `Set` values
        #' @return elements of the set as a base list
        values = function() {
            l = private$elems
            names(l) = NULL
            res = if (length(l))
                unlist(l, recursive = FALSE)
            else
                l

            if (!any(nzchar(names(res))))
                names(res) <- NULL

            res
        }
    ),
    private = list(
        deep_clone = function(name, value) {
            if (name != "elems")
                return(value)

            clone_deep_if_container = function(x) {
                elem = x[[1]]
                elem = if (is.container(elem))
                    elem$clone(deep = TRUE) else elem

                l = list(elem)
                names(l) = names(x)
                l
            }
            lapply(value, clone_deep_if_container)
        },

        .get_hash_value = function(x) {
            tmp = tempfile()
            on.exit(file.remove(tmp))
            saveRDS(x, tmp)
            tools::md5sum(tmp)
        },

        .rename = function(old, new) {
            pos = match(old, names(self))
            names(private$elems[[pos]]) = new
        },

        .replace_value_at = function(pos, value, name) {
            self$discard_at(pos)
            self$add(value, name)
            self
        }
    ),
    lock_class = TRUE
)


#' OrderedSet Class
#'
#' @description The [OrderedSet] is a [Set] where all elements are always
#' ordered.
#' @details The order of elements is determined sequentially as follows:
#' * element's length
#' * whether it is an atomic element
#' * the element's class(es)
#' * by numeric value (if applicable)
#' * it's representation when printed
#' * the name of the element in the [Set]
#'
#' @seealso [Container], [Set]
#' @export
#' @examples
#' s1 = OrderedSet$new(2, 1)
#' s1
OrderedSet <- R6::R6Class("OrderedSet",
    inherit = Set,
    public = list(
        #' @description `OrderedSet` constructor
        #' @param ... initial elements put into the `OrderedSet`
        #' @return returns the `OrderedSet` object
        initialize = function(...) {

            super$initialize()

            it = iter(list(...), .subset = .subset)
            while (has_next(it)) {
                value = get_next(it)
                self$add(value[[1]], name = names(value))
            }

            self
        },

        #' @description Add element
        #' @param value value of `ANY` type to be added to the `OrderedSet`.
        #' @param name `character` optional name attribute of the value.
        #' @return the `OrderedSet` object.
        add = function(value, name = NULL) {

            len = self$length()
            super$add(value, name)

            hasAdded = len < self$length()
            if (hasAdded)
                private$.reorder_values()

            self
        }
    ),
    private = list(
        .reorder_values = function() {
            v = self$values()
            lens = lengths(v)
            classes = paste(lapply(v, class))
            labs = sapply(v, .get_label)
            labnames = names(labs)

            atom = sapply(v, is.atomic)

            inum = which(sapply(v, is.numeric) & lens == 1)
            numbers = numeric(self$length())
            numbers[inum] = as.numeric(v[inum])

            orderCriteria = list(lens,
                                 !atom,
                                 classes,
                                 numbers,
                                 labs,
                                 names(labs))
            orderCriteria = Filter(function(x) length(x) > 0, orderCriteria)

            new_order = do.call(order, args = orderCriteria)
            private$elems = private$elems[new_order]
        }
    ),
    lock_class = TRUE
)


