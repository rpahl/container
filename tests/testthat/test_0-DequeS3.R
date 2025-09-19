
describe("deque",
{
    ee <- expect_equal

    test_that("it creates an empty deque with the expected class attributes",
    {
        d <- deque()
        expect_true(is.deque(d))
        ee(length(d), 0)
        ee(names(d), NULL)
        ee(attr(d, "class"), c("Deque", "Container", "Iterable", "R6"))
    })

    test_that("deque elements can be named",
    {
        d <- deque(a = 2, 9, b = 1)
        ee(names(d), c("a", "", "b"))
    })

    test_that("deque of deque is also a copy throughout",
    {
        d1 <- deque(1)
        d2 <- deque(d1)
        d1$clear()
        ee(d2, deque(deque(1)))
    })
})


describe("as.deque",
{
    ee <- expect_equal

    test_that("as.deque converts various objects to deque",
    {
        ee(as.deque(numeric()), deque())
        ee(as.deque(NULL), deque())
        ee(as.deque(list()), deque())
        ee(as.deque(1), deque(1))
        ee(as.deque(1:2), deque(1L, 2L))
        ee(as.deque(deque(1)), deque(1))
    })

    test_that("as.deque creates a copy from another deque",
    {
        d <- deque(1, 2)
        d2 <- as.deque(d)
        ee(d, d2)
        d$clear()
        # if d2 is a copy, it should not have been cleared
        ee(length(d), 0)
        d2_was_also_cleaned = length(d2) == 0
        expect_false(d2_was_also_cleaned)
    })

    test_that("a data.frame can be converted to a deque",
    {
        daf <- data.frame(A = 1:2, B = 3:4)
        ee(as.list(as.deque(daf)), as.list(daf))
    })

    test_that("a deque can be converted to a list",
    {
        d <- deque(1, b = 2)
        ee(as.list(as.deque(d)), list(1, b = 2))
    })

    test_that("a set can be converted to a deque",
    {
        s <- setnew(1, 2)
        ee(as.list(as.deque(s)), list(1, 2))
    })

    test_that("a dict can be converted to a deque",
    {
        d <- dict(a = 1, b = 2)
        ee(as.list(as.deque(d)), list(a = 1, b = 2))
    })
})


describe("is.deque",
{
    it("can be checked if an object is a deque",
    {
        expect_true(is.deque(deque()))
        expect_false(is.deque(list()))
        expect_false(is.deque(container()))
        expect_false(is.deque(dict()))
        expect_false(is.deque(setnew()))
        expect_true(is.deque(deque(1, 2, 3)))
        expect_true(is.deque(deque(NULL)))
    })

    test_that("is.deque returns an error if no argument is provided",
    {
        expect_error(is.deque())
    })
})


describe("c.Deque",
{
    # nolint start
    ee <- expect_equal

    test_that("if passed one deque, it returns that deque",
    {
        ee(c(deque()), deque())
        ee(c(deque(1)), deque(1))
    })

    describe("non-recursive",
    {
        test_that(
            "default non-recursive concatenation is consistent
             with list concatenation",
        {
            ee(
                as.list(c(deque(), deque())),
                        c( list(),  list())
            )
            ee(
                as.list(c(deque(1), deque())),
                        c( list(1),  list())
            )
            ee(
                as.list(c(deque(1), deque(2))),
                        c( list(1),  list(2))
            )
            ee(
                as.list(c(deque(1), deque(2, list(a = 3)))),
                        c( list(1),  list(2, list(a = 3)))
            )
            ee(
                as.list(c(deque(1), deque(2, deque(a = 3)))),
                        c( list(1),  list(2, deque(a = 3)))
            )
        })

        test_that("deque can be concatenated with objects of other classes",
        {
            ee(
                c(deque(1), list(b = 2), container(c = 3), dict(d = 4)),
                  deque(1,       b = 2,            c = 3,       d = 4)
            )
        })

        test_that("names can be ignored",
        {
            cu <- function(...) c(..., use.names = FALSE)

            ee(
                cu(deque(1), deque(a = 2, b = deque(a = 3))),
                   deque(1,            2,     deque(a = 3))
            )

            ee(
                as.list(cu(a = deque(1), b = deque(2, list(a = 3)))),
                        cu(     list(1),      list(2, list(a = 3))))
        })

        test_that("concatenated objects are copies",
        {
            c1 <- deque(1)
            c2 <- deque(2)
            c1c1 <- deque(c1 = c1)

            cc <- c(c1, c1c1, c2)
            ee(unpack(cc), c(1, c1 = 1, 2))
            c1$add(2)
            ee(unpack(cc), c(1, c1 = 1, 2)) # still the same
        })
    })

    describe("recursive",
    {
        test_that(
            "recursive concatenation is consistent with list concatenation",
        {
            cr <- function(...) c(..., recursive = TRUE)

            ee(cr(deque()), cr(list()))
            ee(cr(deque(1)), cr(list(1)))
            ee(cr(deque(NULL)), cr(list(NULL)))

            ee(cr(deque(), deque()), cr(list(), list()))
            ee(cr(deque(1), deque()), cr(list(1), list()))
            ee(cr(deque(1), deque(2)), cr(list(1), list(2)))
            ee(cr(deque(1), deque(2, 3)), cr(list(1), list(2, 3)))
            ee(
                cr(deque(1), deque(2, list(a = 3))),
                cr( list(1),  list(2, list(a = 3)))
            )
            ee(
                cr(deque(1), deque(2, deque(a = 3))),
                cr( list(1),  list(2,  list(a = 3)))
            )
            ee(
                cr(deque(1), list(2, deque(a = 3))),
                cr( list(1), list(2,  list(a = 3)))
            )
            ee(
                cr(deque(1), list(2, dict(a = 3))),
                cr( list(1), list(2, list(a = 3)))
            )
            ee(
                cr(deque(), list(2, dict(a = 3))),
                cr(list(),  list(2, list(a = 3)))
            )
        })

        test_that(
            "names are combined recursively as expected",
        {
            ee(
                cr(deque(1), deque(a = 2, b = deque(a = 3))),
                cr( list(1),  list(a = 2, b =  list(a = 3)))
            )
            ee(
                cr(deque(1), deque(a = 2, b = deque(a = 3))),
                c(1, a = 2, b.a = 3)
            )
        })
    })
    # nolint end
})


describe("rev",
{
    ee <- expect_equal

    test_that("it reverses the deque without modifying the original", {
        v <- 1:5
        d <- as.deque(v)
        ee(unpack(d), v)

        ee(unpack(rev(d)), rev(v))
        d_was_changed <- !identical(unpack(d), v)
        expect_false(d_was_changed)
    })

    test_that("ref_rev modifies the original deque in place", {
        v <- 1:5
        d <- as.deque(v)
        ee(unpack(d), v)

        ee(unpack(ref_rev(d)), rev(v))
        ee(unpack(d), rev(v))
    })
})
