describe("addleft", {
    ee <- expect_equal

    describe("addleft.Deque", {
        test_that("addleft works for Deque objects", {
            d <- deque(1, 2, 3)
            ee(add(d, n = 0, n1 = 1), deque(1, 2, 3, n = 0, n1 = 1))
            ee(addleft(d, n0 = 0, n1 = 1),
                       deque(n1 = 1, n0 = 0, 1, 2, 3))
            d_was_not_touched <- all.equal(d, as.deque(1:3))
            expect_true(d_was_not_touched)

            ref_addleft(d, 4)
            ee(d, deque(4, 1, 2, 3))
        })
    })
})
