describe("discard_at", {
    ee <- expect_equal

    describe("discard_at.Container", {
        test_that("discard_at works for Container objects", {
            co <- container(a = 1, b = 2, f = mean, 3)
            co2 <- clone(co)
            ee(discard_at(co), co2)
            ee(discard_at(co, "a"), container(b = 2, f = mean, 3))
            original_was_not_touched <- all.equal(co, co2)
            expect_true(original_was_not_touched)

            ee(discard_at(co, "a"), discard_at(co, 1))
            ee(discard_at(co, "b"), discard_at(co, 2))
            ee(discard_at(co, 1:4), container())

            ee(discard_at(co, "b", "a", 4:3, 1), container())
            ee(discard_at(co, "a", 1), discard_at(co, 1))

            ee(discard_at(co, "a", "x"), discard_at(co, "a"))
            ee(discard_at(co, "x", "a"), discard_at(co, "a"))

            ee(discard_at(co, 1, 2, 7), discard_at(co, 1, 2))
        })
    })

    describe("discard_at.Dict", {
        test_that("discard_at works for dict.table objects", {
            d <- dict.table(a = 1, b = 2, f = mean)
            d2 <- clone(d)
            expect_true(is_empty(discard_at(d, 1, "b", 3)))
            expect_true(is_empty(discard_at(d, 1:3)))
            expect_true(is_empty(discard_at(d, 3:1)))
            ee(d, d2)

            # args as character vector
            expect_true(is_empty(discard_at(d, colnames(d))))
            expect_true(is_empty(discard_at(d, rev(colnames(d)))))
            ee(d, d2)

            expect_silent(ref_discard_at(d, "x", 4, 11))
            d_was_not_altered <- all.equal(d, d2)
            expect_true(d_was_not_altered)

            ee(ref_discard_at(d, "b"), d2[, c(1, 3)])
            expect_silent(ref_discard_at(d, "a"))
            expect_false(ncol(d) == ncol(d2))
        })
    })
})
