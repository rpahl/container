describe("Set R6 methods", {
    ee <- expect_equal

    describe("initialize", {
        test_that("Set can be initialized correctly", {
            s <- Set$new()
            expect_true(s$is_empty())
            ee(s$length(), 0)
            expect_false(s$has(NULL))
            ee(attr(s, "class"), c("Set", "Container", "Iterable", "R6"))

            ee(Set$new(1, 1, 1), Set$new(1))
            ee(Set$new(NULL, NULL), Set$new(NULL))
            ee(Set$new(mean, mean, 1, 2), Set$new(mean, 1, 2))

            # Set elements can be named
            s <- Set$new(a = 1, b = 3)
            ee(names(s$values()), c("a", "b"))
        })
    })

    describe("add", {
        test_that("add works correctly for Set objects", {
            # adding zero-length elements works as expected
            s <- Set$new(NULL)
            ee(s$length(), 1)
            s$add(NULL) # is not added twice
            ee(s$length(), 1)
            ee(as.list(s$values()), list(NULL))

            s$add(list())
            expect_true(setequal(as.list(s$values()), list(list(), NULL)))
            s$add(list())
            expect_true(setequal(as.list(s$values()), list(list(), NULL)))

            s$add(numeric(0))
            expect_true(setequal(as.list(s$values()), list(list(), NULL, numeric())))
        })
    })

    describe("at", {
        test_that("at works correctly for Set objects", {
            s <- Set$new(a = 1, 2, b = 3, 4)
            ee(s$at(1), Set$new(a = 1))
            ee(s$at(2), Set$new(2))
            ee(s$at(c("a", "b")), Set$new(a = 1, b = 3))
            ee(s$at(list(1, "b")), Set$new(a = 1, b = 3))

            ee(s$at(1:2), Set$new(a = 1, 2))
            ee(s$at("a"), s$at(match("a", names(s))))
            ee(s$at("b"), s$at(match("b", names(s))))
        })
    })

    describe("at2", {
        test_that("at2 works correctly for Set objects", {
            s <- Set$new(a = 1, 2, b = 3, 4)
            ee(s$at2(1), 1)
            ee(s$at2(2), 2)
            ee(s$at2("a"), 1)

            expect_error(s$at2(as.numeric(NA)), "index must not be 'NA'")
            expect_error(s$at2(c("a", "b")), "index must be of length 1")
            expect_error(s$at2("c"), "index 'c' not found")
        })
    })

    describe("clear", {
        test_that("clear works correctly for Set objects", {
            s <- Set$new(1, 2, 3)
            ee(s$clear(), Set$new())
        })
    })

    describe("delete", {
        test_that("delete works correctly for Set objects", {
            co <- Container$new(1, 2)
            s2 <- Set$new(3, 4)
            s <- Set$new(1, 2, 3, co, s2)
            s$delete(1)
            ee(s, Set$new(2, 3, co, s2))
            s$delete(co)
            ee(s, Set$new(2, 3, s2))
            expect_error(s$delete(co))
            s$delete(s2)
            ee(s, Set$new(2, 3))
        })
    })

    describe("discard", {
        test_that("discard works correctly for Set objects", {
            co <- Container$new(1, 2)
            s2 <- Set$new(3, 4)
            s <- Set$new(1, 2, 3, co, s2)
            s$discard(1)
            ee(s, Set$new(2, 3, co, s2))
            s$discard(co)
            ee(s, Set$new(2, 3, s2))
            expect_silent(s$discard(co))
            s$discard(s2)
            ee(s, Set$new(2, 3))
        })
    })

    describe("discard_at", {
        test_that("discard_at works correctly for Set objects", {
            co <- Container$new(1, 2)
            s2 <- Set$new(3, 4)
            s <- Set$new(1, 2, 3, co = co, s = s2)
            ee(s$discard_at(1), Set$new(2, 3, co = co, s = s2))

            ee(s$discard_at("co"), Set$new(2, 3, s = s2))
            expect_silent(s$discard_at("co"))
            ee(s$discard_at("s")$values(), list(2, 3))
        })
    })

    describe("empty", {
        test_that("empty status is correctly detected", {
            expect_true(Set$new()$is_empty())
        })
    })

    describe("Set operations", {
        test_that("Set operations work correctly", {
            s0   <- Set$new()
            s1   <- Set$new(1)
            s12  <- Set$new(1, 2)
            s23  <- Set$new(   2, 3)
            s1_3 <- Set$new(1,    3)
            s123 <- Set$new(1, 2, 3)

            # diff
            expect_error(s1$diff(2), "arg must be a Set")
            expect_error(s1$diff(NULL), "arg must be a Set")
            expect_error(s1$diff(NA), "arg must be a Set")

            ee(s0$diff(s1), s0)
            ee(s1$diff(s0), s1)
            ee(s123$diff(s0), s123)

            ee(Set$new(1)$diff(s1), s0)
            ee(Set$new(1)$diff(s12), s0)
        })
    })
})
