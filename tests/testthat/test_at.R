describe("at", {
    ee <- expect_equal

    describe("at.Container", {
        test_that("at works for Container objects", {
            co <- container(a = 1, 2, b = 3, 4)
            ee(at(co, 1), container(a = 1))
            ee(at(co, 2), container(2))
            ee(at(co, "a"), container(a = 1))
            expect_error(at(container(), 1),
                        "index 1 exceeds length of Container, which is 0")
            expect_error(at(co, "x"), "index 'x' not found")

            ee(at(co, 1:3), as.container(as.list(co)[1:3]))
            ee(at(co), co)
            ee(at(co, NULL), container())

            ee(at(co, list("a", "b")), at(co, "a", "b"))
            expect_error(at(co, list("a", "b", "x")), "index 'x' not found")
        })
    })

    describe("at.Dict", {
        test_that("at works for Dict objects", {
            d <- dict(a = 1, b = 1:3)
            ee(at(d, 1, "b"), d)
            ee(at(d, "b"), dict(b = 1:3))
            ee(at(d, 1), dict(a = 1))
            ee(at(d, 2), dict(b = 1:3))

            expect_error(at(d, "x"), "index 'x' not found")
            expect_error(at(d, 3, .default = 1),
                        "index 3 exceeds length of Dict, which is 2")
            ee(at(d, list("a", "b")), d)
        })
    })

    describe("at.dict.table", {
        test_that("at works for dict.table objects", {
            dit <- dict.table(a = 1:3, b = 4:6)

            expect_error(at(dit, "a", "a"), "duplicated keys")
            expect_error(at(dit, 1, 1), "duplicated keys")
            expect_error(at(dit, "x"), "index 'x' not found")
            expect_error(at(dit, 1:3),
                        "index 3 exceeds length of dict.table, which is 2")

            ee(at(dit), dit)
            ee(at(dit, NULL), dict.table())

            ee(at(dit, 1:2), dit)
            ee(at(dit, list("a", 2)), dit)
            ee(at(dit, "a"), dict.table(a = 1:3))
            ee(at(dit, 1), dict.table(a = 1:3))
        })
    })
})