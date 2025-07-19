
describe("dict",
{
    d <- dict()

    test_that("dict is initialized correctly",
    {
        expect_true(is.dict(d))
        expect_equal(length(d), 0)
        expect_equal(names(d), NULL)
        expect_equal(attr(d, "class"), c("Dict", "Container", "Iterable", "R6"))

        expect_true(is_empty(dict()))
        expect_equal(mode(as.list(dict())), "list")
    })

    test_that("signals bad init",
    {
        expect_error(dict(1:2), "all elements must be named")
        expect_error(dict(x = 1, y = 2, x = 3), "duplicated keys")
    })

    test_that("dict of dict is initialized as a copy",
    {
        d1 <- dict(a = 1)
        d2 <- dict(d = d1)
        d1$clear()
        d2 |> equals(dict(d = dict(a = 1))) |> expect_true()
    })
})

describe("as.dict",
{
    test_that("a dict is returned as is",
    {
        d <- dict(a = 1, b = 2)
        as.dict(d) |> equals(d) |> expect_true()
    })

    test_that("converts atomic objects as expected",
    {
        as.dict(NULL) |> equals(dict()) |> expect_true()

        as.dict(numeric()) |> equals(dict()) |> expect_true()
        as.dict(c(a = 1)) |> equals(dict(a = 1)) |> expect_true()

        as.dict(character()) |> equals(dict()) |> expect_true()
        as.dict(c(a = "foo")) |> equals(dict(a = "foo")) |> expect_true()

        as.dict(logical()) |> equals(dict()) |> expect_true()
        as.dict(c(a = TRUE)) |> equals(dict(a = TRUE)) |> expect_true()
    })

    test_that("converts a list as expected",
    {
        as.dict(list()) |> equals(dict()) |> expect_true()
        as.dict(list(a = 1, b = "foo")) |>
            equals(dict(a = 1, b = "foo")) |>
            expect_true()
    })

    test_that("a data frame can be converted to a dict",
    {
        df <- data.frame(A = 1:2, B = 3:4)
        as.dict(df) |> equals(dict(A = 1:2, B = 3:4)) |> expect_true()
    })

    test_that("a dict can be converted to a list",
    {
        d <- dict(a = 1, b = 2)
        expect_equal(as.list(d), list(a = 1, b = 2))
    })

    test_that(
        "a set can be converted to a dict if elements are uniquely named",
    {
        s <- setnew(a = 1, b = 2)
        as.dict(s) |> equals(dict(a = 1, b = 2)) |> expect_true()

        s <- setnew(a = 1, a = 2)
        expect_error(as.dict(s), "duplicated keys are not allowed")

        s <- setnew(1, 2)
        expect_error(as.dict(s), "all elements must be named")
    })
})


describe("is.dict",
{
    test_that("is.dict works as expected",
    {
        expect_true(is.dict(dict()))
        expect_true(is.dict(dict(a = 1)))
        expect_true(is.dict(dict(a = NULL)))

        expect_false(is.dict(0))
        expect_false(is.dict(list()))
        expect_false(is.dict(container()))
        expect_false(is.dict(setnew()))
        expect_false(is.dict(deque()))
    })

    test_that("is.dict signals an error for no input",
    {
        expect_error_fixed(is.dict(), "argument \"x\" is missing")
    })
})


describe("c.Dict",
{
    test_that("the concatenation is a copy of the input",
    {
        d1 <- dict(a = 1)
        d2 <- dict(b = 2)
        cc <- c(d1, d2)

        d1$add("x", 3)
        still_the_same <- equals(unpack(cc), c(a = 1, b = 2))
        expect_true(still_the_same)
    })

    test_that(
        "standard non-recursive concatenation works same as for base lists",
    {
        expect_equal(as.list(c(dict())), c(list()))
        expect_equal(as.list(c(dict(x = 1))), c(list(x = 1)))
        expect_equal(as.list(c(dict(x = NULL))), c(list(x = NULL)))
        expect_equal(as.list(c(dict(), dict())), c(list(), list()))

        expect_equal(as.list(c(dict(a = 1), dict())), c(list(a = 1), list()))
        expect_equal(
            as.list(c(dict(a = 1), dict(b = 2))),
            c(list(a = 1), list(b = 2))
        )
        expect_equal(
            as.list(c(dict(a = 1), dict(b = 2, l = list(a = 3)))),
            c(list(a = 1), list(b = 2, l = list(a = 3)))
        )

        as.list(c(dict(a = 1), d = dict(b = 2, d = dict(a = 3)))) |>
            equals(c(list(a = 1), d = list(b = 2, d = dict(a = 3)))) |>
            expect_true()

        c(dict(a = 1), dict(x = 2, b = dict(a = 3))) |>
            equals(dict(a = 1, x = 2, b = dict(a = 3))) |>
            expect_true()
    })

    test_that(
        "recursive concatenation works same as for base lists",
    {
        cr <- function(...) c(..., recursive = TRUE)
        ee <- expect_equal

        ee(cr(dict()), NULL)
        ee(cr(dict()), cr(list()))

        ee(cr(dict(x = NULL)), NULL)
        ee(cr(dict(x = NULL)), cr(list(x = NULL)))

        ee(cr(dict(a = 1)), c(a = 1))
        ee(cr(dict(a = 1)), cr(list(a = 1)))


        ee(cr(dict(), dict()), NULL)
        ee(cr(dict(), dict()), cr(list(),  list()))

        ee(cr(dict(a = 1), dict()), c(a = 1))
        ee(cr(dict(a = 1), dict()), cr(list(a = 1), list()))


        ee(cr(dict(a = 1), dict(b = 2)), c(a = 1, b = 2))
        ee(cr(dict(a = 1), dict(b = 2)), cr(list(a = 1), list(b = 2)))

        ee(cr(dict(a = 1), dict(b = 2, c = 3)), c(a = 1, b = 2, c = 3))
        ee(
            cr(dict(a = 1), dict(b = 2, c = 3)),
            cr(list(a = 1), list(b = 2, c = 3))
        )

        ee(
            cr(dict(a = 1), dict(b = 2, l = list(a = 3))),
            c(a = 1, b = 2, l.a = 3)
        )
        ee(
            cr(dict(a = 1), dict(b = 2, l = list(a = 3))),
            cr(list(a = 1), list(b = 2, l = list(a = 3)))
        )

        ee(
            cr(dict(a = 1), dict(b = 2, d = dict(a = 3))),
            c(a = 1, b = 2, d.a = 3)
        )
        ee(
            cr(dict(a = 1), dict(b = 2, d = dict(a = 3))),
            cr(list(a = 1), list(b = 2, d = list(a = 3)))
        )


        ee(
            cr(dict(a = 1), list(b = 2, d = dict(a = 3))),
            c(a = 1, b = 2, d.a = 3)
        )
        ee(
            cr(dict(a = 1), list(b = 2, d = dict(a = 3))),
            cr(list(a = 1), list(b = 2, d = list(a = 3)))
        )

        ee(
            cr(dict(a = 1), l = list(b = 2, d = dict(a = 3))),
            c(a = 1, l.b = 2, l.d.a = 3)
        )
        ee(
            cr(dict(a = 1), l = list(b = 2, d = dict(a = 3))),
            cr(list(a = 1), l = list(b = 2, d = list(a = 3)))
        )

        ee(
            cr(dict(), l = list(b = 2, x = dict(a = 3))),
            c(l.b = 2, l.x.a = 3)
        )
        ee(
            cr(dict(), l = list(b = 2, x = dict(a = 3))),
            cr(list(), l = list(b = 2, x = list(a = 3)))
        )
    })
})
