describe(
    "rev functions",
{
    describe(
        "rev.Deque",
    {
        test_that("rev works correctly on Deque",
        {
            v <- 1:5
            d <- as.deque(v)
            expect_equal(unpack(d), v)

            expect_equal(unpack(rev(d)), rev(v))
            d_was_changed <- !identical(unpack(d), v)
            expect_false(d_was_changed)

            expect_equal(unpack(ref_rev(d)), rev(v))
            expect_equal(unpack(d), rev(v))
        })
    })
})
