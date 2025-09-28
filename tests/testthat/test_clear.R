describe("clear", {
    ee <- expect_equal

    describe("clear.Container", {
        test_that("clear works for Container objects", {
            co <- container(1, 2, mean)
            ee(clear(co), container())
            expect_false(is_empty(co))
            ee(ref_clear(co), container())
            expect_true(is_empty(co))
        })
    })

    describe("clear.dict.table", {
        test_that("clear works for dict.table objects", {
            dit <- dict.table(a = 1, b = 2)
            ee(clear(dit), dict.table())
            expect_false(is_empty(dit))
            ee(ref_clear(dit), dict.table())
            expect_true(is_empty(dit))
        })
    })
})