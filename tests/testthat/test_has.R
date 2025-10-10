describe("has",
{
    ee <- expect_equal

    test_that("has.Container works correctly", {
        co <- container(1, 2, mean)
        expect_true(has(co, 1))
        expect_true(has(co, mean))
        expect_false(has(co, 1:2))
        expect_false(has(co, NA))
        expect_false(has(co, NULL))
    })

    test_that("has.Dict works correctly", {
        d <- dict(a = 1, b = 3)
        expect_false(has(d, "a"))
        expect_true(has(d, 1))
        expect_false(has(d, 2))
    })

    test_that("has.dict.table works correctly", {
        dit <- dict.table(a = 1:3, b = as.list(4:6))

        expect_true(has(dit, 1:3))
        expect_false(has(dit, 4:6))
        expect_true(has(dit, as.list(4:6)))

        expect_warning(has(dit, 1:2),
            "length of column vector \\(2\\) does not match number of rows \\(3\\)")
    })
})
