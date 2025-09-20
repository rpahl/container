describe(
    "peek_at functions",
{
    ee <- expect_equal

    describe(
        "peek_at.Container",
    {
        test_that("peek_at works correctly on Container",
        {
            co <- container(a = 1, 2, b = 3, 4)
            ee(peek_at(co, 1), container(a = 1))
            ee(peek_at(co, 2), container(2))
            ee(peek_at(co, "a"), container(a = 1))
            ee(peek_at(container(), 1), container())
            ee(peek_at(container(), 1, .default = 0), container(0))
            ee(peek_at(co, "x"), container())
            ee(peek_at(co, "x", .default = 1), container(x = 1))
            ee(peek_at(co, "x", "y", .default = 1), container(x = 1, y = 1))

            ee(peek_at(co, "x", .default = 1:3), container(x = 1:3))
            ee(peek_at(co, 1, 2, "x", "b", .default = 1:3),
               container(a = 1, 2, x = 1:3, b = 3))

            ee(peek_at(co, 1:3), as.container(as.list(co)[1:3]))

            ee(peek_at(co), co)
            ee(peek_at(co, .default = 1), co)
        })

        test_that("peek_at handles edge cases correctly",
        {
            co <- container(a = 1, 2, b = 3, 4)
            # Edge cases
            ee(peek_at(co, NA), container())
            ee(peek_at(co, integer()), container())
            ee(peek_at(co, TRUE), container())
            ee(peek_at(co, NULL), container())
        })
    })

    describe(
        "peek_at.Dict",
    {
        test_that("peek_at works correctly on Dict",
        {
            d <- dict(a = 1, b = 1:3)

            expect_error(peek_at(d, "a", "a"), "duplicated keys")
            expect_error(peek_at(d, 1, 1), "duplicated keys")
            ee(peek_at(d), d)
            ee(peek_at(d, NULL), dict())

            ee(peek_at(d, 1:2, "x", "y"), d)

            ee(peek_at(d, "b"), dict(b = 1:3))
            ee(peek_at(d, "x"), dict())
            ee(peek_at(d, 1), dict(a = 1))
            ee(peek_at(d, 2), dict(b = 1:3))
            ee(peek_at(d, "x", .default = 4:6), dict(x = 4:6))
            ee(peek_at(d, 1, "x", "b", .default = 4:6),
               dict(a = 1, b = 1:3, x = 4:6))

            expect_error(peek_at(d, 3, .default = 1), "all elements must be named")
        })

        test_that("peek_at works with lists on Dict",
        {
            d <- dict(a = 1, b = 1:3)
            ee(peek_at(d, list("a", "b", z = 9)), d)

            ee(peek_at(d, list("a", "b", z = 9), .default = 9), c(d, dict(z = 9)))
            # The above does not work if specified outside a list unless default is NULL
            expect_error(peek_at(d, "a", "b", z = 9, .default = 9),
                         "all elements must be named")
            ee(peek_at(d, "a", "b", z = 9), d)
        })
    })

    describe(
        "peek_at.dict.table",
    {
        test_that("peek_at works correctly on dict.table",
        {
            dit <- dict.table(a = 1:3, b = 4:6)

            expect_error(peek_at(dit, "a", "a"), "duplicated keys")
            expect_error(peek_at(dit, 1, 1), "duplicated keys")
            ee(peek_at(dit), dit)
            ee(peek_at(dit, NULL), dict.table())

            ee(peek_at(dit, 1:2, "x", "y"), dit)
            ee(peek_at(dit, "a"), dict.table(a = 1:3))
            ee(peek_at(dit, 1), dict.table(a = 1:3))

            ee(peek_at(dit, 3), dict.table())
            ee(peek_at(dit, "x"), dict.table())

            ee(peek_at(dit, "x", .default = 0), dict.table(x = rep(0, nrow(dit))))

            ee(peek_at(dit, "a", "x", .default = 0),
               dict.table(a = 1:3, x = rep(0, nrow(dit))))

            ee(peek_at(dit, 1, "x", "b", .default = 0),
               cbind(dit, dict.table(x = c(0, 0, 0))))
        })

        test_that("peek_at works with lists on dict.table",
        {
            dit <- dict.table(a = 1:3, b = 4:6)
            ee(peek_at(dit, list(x = 9)), dict.table())
            ee(peek_at(dit, list(x = 9), .default = -1), dict.table(x = rep(-1, 3)))
            ee(peek_at(dit, list(x = "a")), dit[, 1])

            ee(peek_at(dit, list("a", "b", z = 9), .default = 9),
               cbind(dit, dict.table(z = c(9, 9, 9))))
            # The above does not work if specified outside a list unless default is NULL
            expect_error(peek_at(dit, "a", "b", z = 9, .default = 9),
                         "all elements must be named")
            ee(peek_at(dit, "a", "b", z = 9), dit)
        })

        test_that("peek_at handles length mismatches with warnings",
        {
            dit <- dict.table(a = 1:3, b = 4:6)
            suppressWarnings({
                ee(peek_at(dit, "x", .default = 1:2), dict.table(x = c(1, 2, 1)))
            })
            expect_warning(peek_at(dit, "x", .default = 1:2),
                           "did not match number of rows")
        })
    })
})