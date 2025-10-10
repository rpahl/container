describe("utility functions",
{
    test_that(".is_string works correctly", {
        f <- container:::.is_string
        expect_false(f(1))
        expect_false(f(NULL))
        expect_false(f(NA))
        expect_false(f(character()))
        expect_false(f(as.character(NA)))
        expect_false(f(c("a", "b")))
        expect_true(f(""))
        expect_true(f("a"))
        expect_true(f("1"))
    })

    test_that(".is_nonempty_string works correctly", {
        f <- container:::.is_nonempty_string
        expect_false(f(1))
        expect_false(f(NULL))
        expect_false(f(NA))
        expect_false(f(character()))
        expect_false(f(as.character(NA)))
        expect_false(f(c("a", "b")))
        expect_false(f(""))
        expect_true(f("a"))
        expect_true(f("1"))
    })

    test_that(".verify_names works correctly", {
        f <- container:::.verify_names
        expect_error(f(NULL))
        expect_error(f(""))
        expect_error(f(c(NA, NA)))
        expect_true(f("a"))
    })

    test_that(".check_name_collision works correctly", {
        ee <- expect_error
        f <- container:::.check_name_collision
        expect_true(f("a", "b"))
        ee(f("a", "a"), "name 'a' exists already")
        ee(f(c("a", "b"), c("b", "c")), "name 'b' exists already")
        ee(f(c("a", "b"), c("b", "a")),
           "names 'a', 'b' exist already")
    })

    test_that("unlist1 works correctly", {
        f <- container:::unlist1

        # unlist1 unravels a list by one level
        l <- list(a = list(x = list(1, 2)))

        expect_equal(as.numeric(unlist(l)), 1:2)
        expect_equal(f(l), list(a.x = list(1, 2)))
        expect_equal(f(l, use.names = FALSE), list(list(1, 2)))
    })
})
