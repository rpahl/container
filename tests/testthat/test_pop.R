describe(
    "pop functions",
{
    ee <- expect_equal

    describe(
        "pop_.Deque",
    {
        test_that("pop functions work correctly on Deque",
        {
            d <- deque(1, 2, 3)
            ee(ref_pop(d), 3)
            ee(ref_popleft(d), 1)
            ee(d, deque(2))

            expect_error(ref_pop(deque()), "pop at empty Deque")
            expect_error(ref_popleft(deque()), "popleft at empty Deque")
        })
    })

    describe(
        "ref_pop.Container",
    {
        test_that("ref_pop works correctly on Container",
        {
            co <- container(a = 1, b = 1:3, d = "foo")

            ee(ref_pop(co, "b"), 1:3)
            ee(ref_pop(co, "a"), 1)
            expect_error(ref_pop(co, "x"), "index 'x' not found")
        })
    })

    describe(
        "ref_pop.Dict",
    {
        test_that("ref_pop works correctly on Dict",
        {
            d <- dict(a = 1, b = 1:3)
            expect_error(ref_pop(d, "x"), "index 'x' not found")

            ee(ref_pop(d, "b"), 1:3)
            ee(ref_pop(d, "a"), 1)
            expect_error(ref_pop(d, "x"), "pop at empty Dict")
        })
    })

    describe(
        "ref_pop.dict.table",
    {
        test_that("ref_pop works correctly on dict.table",
        {
            dit <- dict.table(a = 1:3, b = 4:6)

            ee(ref_pop(dit, "a"), 1:3)
            ee(ref_pop(dit, 1), 4:6)
            expect_error(ref_pop(dit, 1), "index 1 exceeds length of dict.table, which is 0")
            expect_error(ref_pop(dit, "z"), "index 'z' not found")
        })
    })
})
