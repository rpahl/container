describe("at2", {
    ee <- expect_equal

    describe("at2.Container", {
        test_that("at2 works for Container objects", {
            co <- container(a = 1, 2, b = 3, 4)
            ee(at2(co, 1), 1)
            ee(at2(co, 2), 2)
            ee(at2(co, "a"), 1)
            expect_error(at2(container(), 1),
                        "index 1 exceeds length of Container, which is 0")
            expect_error(at2(co), "'index' is missing")
            expect_error(at2(co, 1:2), "index must be of length 1")
            expect_error(at2(co, "x"), "index 'x' not found")
        })
    })

    describe("at2.Dict", {
        test_that("at2 works for Dict objects", {
            d <- dict(a = 1, b = 2)
            ee(at2(d, 1), 1)
            ee(at2(d, 2), 2)
            ee(at2(d, "a"), 1)
            expect_error(at2(dict(), 1), "index 1 exceeds length of Dict, which is 0")
            expect_error(at2(d), "'index' is missing")
            expect_error(at2(d, 1:2), "index must be of length 1")
            expect_error(at2(d, "x"), "index 'x' not found")
        })
    })

    describe("at2.dict.table", {
        test_that("at2 works for dict.table objects", {
            dit <- dict.table(a = 1:3, b = 4:6)
            ee(at2(dit, "a"), 1:3)
            ee(at2(dit, 1), 1:3)
            expect_error(at2(dict.table(), 1),
                        "index 1 exceeds length of dict.table, which is 0")
            expect_error(at2(dit), "'index' is missing")
            expect_error(at2(dit, 1:2), "index must be of length 1")
            expect_error(at2(dit, "x"), "index 'x' not found")
        })
    })
})
