describe("clone", {
    ee <- expect_equal

    describe("clone.Container", {
        test_that("clone works for Container objects", {
            co <- container(1, 2, mean)
            co2 <- clone(co)
            ee(co2, co)
            ref_clear(co)
            expect_true(is_empty(co))
            expect_false(is_empty(co2))
        })
    })

    describe("clone.dict.table", {
        test_that("clone works for dict.table objects", {
            dit <- dict.table(a = 1, b = 2)
            dit2 <- clone(dit)
            ee(dit2, dit)
            ref_clear(dit)
            expect_true(is_empty(dit))
            expect_false(is_empty(dit2))
        })
    })
})