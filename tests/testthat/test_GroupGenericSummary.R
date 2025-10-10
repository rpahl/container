describe("GroupGenericSummary", {
    ee <- expect_equal

    describe("Container", {
        test_that("summary operations work for Container objects", {
            v <- 1:4
            co <- as.container(v)
            ee(sum(co), sum(v))
            ee(prod(co), prod(v))
            ee(min(co), min(v))
            ee(max(co), max(v))
            ee(range(co), range(v))
            expect_true(is.na(sum(container(1, NA, 3))))
            ee(sum(container(1, NA, 3), na.rm = TRUE), 1 + 3)
            expect_true(is.na(min(container(1, NA))))
            ee(min(container(1, NA), na.rm = TRUE), 1)
            expect_true(all(container(TRUE, TRUE, TRUE)))
            expect_false(all(container(TRUE, FALSE, TRUE)))
            expect_true(any(container(TRUE, FALSE, TRUE)))

            ee(sum(container(co, co)), 2 * sum(v))
            ee(range(container(1, 2, container(-10, container(10)), 3)), c(-10, 10))
        })
    })

    describe("Deque", {
        test_that("summary operations work for Deque objects", {
            v <- 1:4
            d <- as.deque(v)
            ee(sum(d), sum(v))
            ee(prod(d), prod(v))
            ee(min(d), min(v))
            ee(max(d), max(v))
            ee(range(d), range(v))
            expect_true(is.na(sum(deque(1, NA, 3))))
            ee(sum(deque(1, NA, 3), na.rm = TRUE), 1 + 3)
            expect_true(is.na(min(deque(1, NA))))
            ee(min(deque(1, NA), na.rm = TRUE), 1)
            expect_true(all(deque(TRUE, TRUE, TRUE)))
            expect_false(all(deque(TRUE, FALSE, TRUE)))
            expect_true(any(deque(TRUE, FALSE, TRUE)))

            ee(sum(deque(d, d)), 2 * sum(v))
            ee(range(deque(1, 2, deque(-10, deque(10)), 3)), c(-10, 10))
        })
    })

    describe("Dict", {
        test_that("summary operations work for Dict objects", {
            v <- 1:4
            names(v) <- letters[1:4]
            d <- as.dict(v)
            ee(sum(d), sum(v))
            ee(prod(d), prod(v))
            ee(min(d), min(v))
            ee(max(d), max(v))
            ee(range(d), range(v))
            expect_true(is.na(sum(dict(a = 1, b = NA, c = 3))))
            ee(sum(dict(a = 1, b = NA, c = 3), na.rm = TRUE), 1 + 3)
            expect_true(is.na(min(dict(a = 1, b = NA))))
            ee(min(dict(a = 1, b = NA), na.rm = TRUE), 1)
            expect_true(all(dict(a = TRUE, b = TRUE, x = TRUE)))
            expect_false(all(dict(a = TRUE, b = FALSE, x = TRUE)))
            expect_true(any(dict(a = TRUE, b = FALSE, x = TRUE)))

            ee(sum(deque(d, d)), 2 * sum(v))
            ee(range(dict(a = 1, b = 2, d1 = dict(x = -10, d2 = dict(y = 10)), z = 3)),
               c(-10, 10))
        })
    })

    describe("Set", {
        test_that("summary operations work for Set objects", {
            v <- 1:4
            s <- as.set(v)
            ee(sum(s), sum(v))
            ee(prod(s), prod(v))
            ee(min(s), min(v))
            ee(max(s), max(v))
            ee(range(s), range(v))
            expect_true(is.na(sum(setnew(1, NA, 3))))
            ee(sum(setnew(1, NA, 3), na.rm = TRUE), 1 + 3)
            expect_true(is.na(min(setnew(1, NA))))
            ee(min(setnew(1, NA), na.rm = TRUE), 1)
            expect_true(all(setnew(TRUE, TRUE, TRUE)))
            expect_false(all(setnew(TRUE, FALSE, TRUE)))
            expect_true(any(setnew(TRUE, FALSE, TRUE)))

            ee(sum(setnew(s, s)), sum(v))
            ee(range(setnew(1, 2, setnew(-10, setnew(10)), 3)), c(-10, 10))
        })
    })
})
