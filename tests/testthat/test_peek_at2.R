describe(
    "peek_at2 functions",
{
    ee <- expect_equal

    describe(
        "peek_at2.Container",
    {
        test_that("peek_at2 works correctly on Container",
        {
            co <- container(a = 1, 2, b = 3, 4)
            ee(peek_at2(co, 1), 1)
            ee(peek_at2(co, 2), 2)
            ee(peek_at2(co, "a"), 1)
            ee(peek_at2(container(), 1), NULL)
            ee(peek_at2(container(), 1, default = 0), 0)
            ee(peek_at2(co), NULL)
            ee(peek_at2(co, default = 1), 1)
        })

        test_that("peek_at2 validates indices correctly on Container",
        {
            co <- container(a = 1, 2, b = 3, 4)
            expect_error(peek_at2(co, 1:2), "index must be of length 1")
            expect_error(peek_at2(co, c("a", "b")), "index must be of length 1")
            expect_error(peek_at2(co, NA), "index must not be 'NA'")
        })
    })

    describe(
        "peek_at2.Dict",
    {
        test_that("peek_at2 works correctly on Dict",
        {
            d <- dict(a = 1, b = 1:3)
            ee(peek_at2(d, "b"), 1:3)
            ee(peek_at2(d, "x"), NULL)
            ee(peek_at2(d, "x", default = 4:6), 4:6)
            ee(peek_at2(d, 1), 1)
            ee(peek_at2(d, 2), 1:3)
            ee(peek_at2(d), NULL)
            ee(peek_at2(d, default = 1), 1)
        })
    })

    describe(
        "peek_at2.Deque",
    {
        test_that("peek_at2 and related functions work correctly on Deque",
        {
            d <- deque(1, 2, 3)
            ee(peek(d), 3)
            ee(peekleft(d), 1)
            ee(peek(deque()), NULL)
            ee(peekleft(deque()), NULL)
            ee(peek(deque(), default = 1), 1)
            ee(peekleft(deque(), default = 0), 0)
            ee(peek_at2(d), NULL)
            ee(peek_at2(d, default = 1), 1)
        })
    })

    describe(
        "peek_at2.dict.table",
    {
        test_that("peek_at2 works correctly on dict.table",
        {
            dit <- dict.table(a = 1:3, b = 4:6)
            ee(peek_at2(dit, "a"), 1:3)
            ee(peek_at2(dit, 1), 1:3)
            ee(peek_at2(dit, 3), NULL)
            ee(peek_at2(dit, "x"), NULL)
            ee(peek_at2(dit, "x", default = 0), rep(0, 3))
        })

        test_that("peek_at2 validates indices correctly on dict.table",
        {
            dit <- dict.table(a = 1:3, b = 4:6)
            expect_error(peek_at2(dit, 1:2), "index must be of length 1")
            expect_error(peek_at2(dit, NA), "index must not be 'NA'")
        })

        test_that("peek_at2 handles length mismatches with warnings",
        {
            dit <- dict.table(a = 1:3, b = 4:6)
            expect_warning(peek_at2(dit, "x", default = 1:2),
                           "did not match number of rows")
        })
    })
})