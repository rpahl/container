describe("replace_at",
{
    ee <- expect_equal
    EE <- expect_error

    test_that("replace_at.Container works with named arguments", {
        x <- container(a = 0, b = "z")

        # named args
        ee(replace_at(x, a = 1, b = "a"), container(a = 1, b = "a"))
        ee(replace_at(x, a = 1:3), container(a = 1:3, b = "z"))
        ee(replace_at(x, a = 1:2, b = list(2, 3)), container(a = 1:2, b = list(2, 3)))
        ee(replace_at(x, a = NULL), container(a = NULL, b = "z"))

        EE(replace_at(x, 1, b = "a"), "all elements must be named")
        EE(replace_at(x, "x" = 2), "names\\(s\\) not found: 'x'")

        ee(replace_at(x, "x" = 2, .add = TRUE), c(x, container("x" = 2)))
        ee(replace_at(x, "x" = 2:4, .add = TRUE), c(x, container("x" = 2:4)))
        ee(replace_at(x, "x" = 2, "a" = 1, .add = TRUE), container(a = 1, b = "z", x = 2))
    })

    test_that("replace_at.Container works with positional arguments", {
        x <- container(a = 0, b = "z")

        # Pair of args
        ee(replace_at(x, 1, 9), container(a = 9, b = "z"))
        ee(replace_at(x, 1, NULL), container(a = NULL, b = "z"))
        ee(replace_at(x, 1, 1:3), container(a = 1:3, b = "z"))
        ee(replace_at(x, "a", 1:3), container(a = 1:3, b = "z"))
        ee(replace_at(x, 2, letters[1:3]), container(a = 0, b = letters[1:3]))
        ee(replace_at(x, "b", letters[1:3]), container(a = 0, b = letters[1:3]))
        ee(replace_at(x, 1, list(1, 2)), container(a = list(1, 2), b = "z"))

        ee(replace_at(x, 1:2, 1:2), container(a = 1, b = 2))
        ee(replace_at(x, list(1, 2), list(1:2, 3:4)), container(a = 1:2, b = 3:4))

        ee(replace_at(x, list("b", 1), 1:2), container(a = 2, b = 1))
        ee(replace_at(x, list("b", 1), list(1:3, 4:6)), container(a = 4:6, b = 1:3))
    })

    test_that("replace_at.Container error handling", {
        x <- container(a = 0, b = "z")

        EE(replace_at(x, "x", 1), "names\\(s\\) not found: 'x'")
        EE(replace_at(x, 1:4, 1:4), "index out of range \\(length = 2\\): 3")
        EE(replace_at(x, 4:1, 4:1), "index out of range \\(length = 2\\): 4")

        EE(replace_at(x, 1:2, 1),
           "length of indices \\(2\\) and values \\(1\\) don't match")

        EE(replace_at(x, list(1, 2), 1),
           "length of indices \\(2\\) and values \\(1\\) don't match")

        EE(replace_at(x, 1:2, as.list(1:3)),
           "length of indices \\(2\\) and values \\(3\\) don't match")

        ee(replace_at(x, "x", 1, .add = TRUE), c(x, container(x = 1)))
        EE(replace_at(x, 1:4, 1:4, .add = TRUE), "index out of range \\(length = 2\\): 3")
    })

    test_that("ref_replace_at.Container modifies in place", {
        x <- container(a = 0, b = "z")

        # Replace by reference
        ee(ref_replace_at(x, 1, 1), container(a = 1, b = "z"))
        was_changed_by_reference <- isTRUE(all.equal(x, container(a = 1, b = "z")))
        expect_true(was_changed_by_reference)

        EE(ref_replace_at(x, 1:3, 1:3), "index out of range \\(length = 2\\): 3")
        x_was_not_touched <- isTRUE(all.equal(x, container(a = 1, b = "z")))
        expect_true(x_was_not_touched)

        # Ensure there are no partial operations done when indices are invalid
        x <- container(a = 0, b = "z")
        EE(ref_replace_at(x, a = 1, x = 1), "names\\(s\\) not found: 'x'")
        x_was_not_touched <- isTRUE(all.equal(x, container(a = 0, b = "z")))
        expect_true(x_was_not_touched)
    })

    test_that("replace_at.dict.table works with named arguments", {
        dit <- dict.table(a = 1:2, b = 3:4)

        # named args
        ee(replace_at(dit, a = 2:1, b = 4:3), dict.table(a = 2:1, b = 4:3))
        ee(replace_at(dit, a = 2:1), dict.table(a = 2:1, b = 3:4))
        ee(replace_at(dit, a = NULL), dict.table(b = 3:4))

        EE(replace_at(dit, 1, b = 4:3), "all elements must be named")
        EE(replace_at(dit, "x" = 2), "column\\(s\\) not found: 'x'")

        ee(replace_at(dit, "x" = 2, .add = TRUE), cbind(dit, dict.table(x = 2)))
        ee(replace_at(dit, a = 1, "x" = 2, .add = TRUE), dict.table(a = 1, b = 3:4, x = 2))

        dit3 <- dict.table(a = 1:2, b = 3:4, c = 5:6)
        ee(replace_at(dit3, a = 0, c = 0), dict.table(a = 0, b = 3:4, c = 0))
        ee(replace_at(dit3, a = 0, x = 0, .add = TRUE),
           dict.table(a = 0, b = 3:4, c = 5:6, x = 0))
    })

    test_that("replace_at.dict.table works with positional arguments", {
        dit <- dict.table(a = 1:2, b = 3:4)

        # Pair of args
        ee(replace_at(dit, "a", 2:1), dict.table(a = 2:1, b = 3:4))
        ee(replace_at(dit, 1, 2:1), replace_at(dit, "a", 2:1))
        ee(replace_at(dit, 1:2, list(2:1, 4:3)), replace_at(dit, a = 2:1, b = 4:3))
        ee(replace_at(dit, list(1, 2), list(2:1, 4:3)), replace_at(dit, a = 2:1, b = 4:3))
        ee(replace_at(dit, list("a", 2), list(2:1, 4:3)), replace_at(dit, a = 2:1, b = 4:3))
        ee(replace_at(dit, list(1, "b"), list(2:1, 4:3)), replace_at(dit, a = 2:1, b = 4:3))

        EE(replace_at(dit, "x", 5:6), "column\\(s\\) not found: 'x'")
        EE(replace_at(dit, 3, 5:6), "index out of range \\(ncol = 2\\): 3")
        EE(replace_at(dit, 2:3, list(2:1, 4:3)), "index out of range \\(ncol = 2\\): 3")

        ee(replace_at(dit, list("a", "x"), list(2:1, 6:5), .add = TRUE),
           dict.table(a = 2:1, b = 3:4, x = 6:5))
        EE(replace_at(dit, 2:3, list(4:3, 5:6), .add = TRUE),
           "index out of range \\(ncol = 2\\): 3")

        dit3 <- dict.table(a = 1:2, b = 3:4, c = 5:6)
        ee(replace_at(dit3, c(1, 3), 1:2), dict.table(a = 1, b = 3:4, c = 2))
        ee(replace_at(dit3, c("a", "c"), 1:2), dict.table(a = 1, b = 3:4, c = 2))
        ee(replace_at(dit3, list(1, "c"), 1:2), dict.table(a = 1, b = 3:4, c = 2))
        ee(replace_at(dit3, list("a", 3), 1:2), dict.table(a = 1, b = 3:4, c = 2))

        EE(replace_at(dit3, c("a", "x"), 1:2), "column\\(s\\) not found: 'x'")
        EE(replace_at(dit3, list("a", 4), 1:2), "index out of range \\(ncol = 3\\): 4")

        ee(replace_at(dit3, c("a", "x"), 1:2, .add = TRUE),
           dict.table(a = 1, b = 3:4, c = 5:6, x = 2))
    })

    test_that("ref_replace_at.dict.table modifies in place", {
        dit <- dict.table(a = 1:2, b = 3:4)

        # Replace by reference
        ee(ref_replace_at(dit, "a", 2:1), dict.table(a = 2:1, b = 3:4))

        was_changed_by_reference <- isTRUE(all.equal(dit, dict.table(a = 2:1, b = 3:4)))
        expect_true(was_changed_by_reference)

        # Ensure there are no partial operations done when indices are invalid
        EE(ref_replace_at(dit, a = 0, x = 2:1))
        is_unchanged <- isTRUE(all.equal(dit, dict.table(a = 2:1, b = 3:4)))
        has_partial_operations <- !is_unchanged
        expect_false(has_partial_operations)
    })
})