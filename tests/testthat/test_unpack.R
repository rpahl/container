describe("unpack",
{
    ee <- expect_equal

    test_that("unpack behaves like unlist for regular lists", {
        # Helper function to verify equality to unlist if only using list arguments
        check <- function(x, ...) {
            ee(unpack(x, ...), unlist(x, ...))
        }

        check(NULL)
        check(numeric())
        check(list())
        check(c(a = 1))
        check(c(a = 1), use.names = FALSE)
        check(list(a = 1))
        check(list(a = 1, 2))
        check(list(a = 1, 2, use.names = FALSE))

        l <- list(a = list(b = 1:5, ll = list(0, x = NULL, 1)))
        check(l)
        check(l, recursive = FALSE)
        check(l, use.names = FALSE)
        check(l, recursive = FALSE, use.names = FALSE)
    })

    test_that("unpack works with container objects", {
        # Helper function to compare container vs list behavior
        check <- function(x, y, ...) {
            ee(unpack(x, ...), unlist(y, ...))
        }

        check(container(), list())
        check(container(), list(), recursive = FALSE)
        check(container(NULL), list(NULL))
        check(container(container()), list(list()))
        check(container(co = container()), list(co = container()), recursive = FALSE)

        daf <- data.frame(A = 1:2, B = 3:4)
        check(container(daf = daf), list(daf = daf))
        check(container(daf = daf), list(daf = daf), use.names = FALSE)
        check(container(daf = daf), list(daf = daf), recursive = FALSE)

        co <- container(a = 1, b = 2)
        check(container(co = co), list(co = co), recursive = FALSE)
        ee(unpack(container(co = co)), c(co.a = 1, co.b = 2))
    })

    test_that("unpack works with nested container structures", {
        # nested containers
        co <- container(co = container(0),
                       s = setnew(1, 2),
                       d = dict(a = 1, b = 9),
                       de = as.deque(5:6))
        ee(unpack(co),
           c(co = 0, s1 = 1, s2 = 2, d.a = 1, d.b = 9, de1 = 5, de2 = 6))
    })
})
