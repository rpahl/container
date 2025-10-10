describe("replace",
{
    ee <- expect_equal

    test_that("replace.default and ref_replace.default work correctly", {
        ee(replace(1:3, 1, 0), c(0, 2, 3))
        ee(replace(list(a = 1, b = 2), "a", 0), list(a = 0, b = 2))
        expect_error(ref_replace(1:3, 1, 0), "no applicable method")
    })

    test_that("replace.Container and ref_replace.Container work correctly", {
        x <- container(1, "z")
        ee(replace(x, 1, 0), container(0, "z"))
        ee(replace(x, "z", 0), container(1, 0))
        ee(replace(x, "z", NULL), container(1, NULL))

        expect_error(replace(x, old = 99, 0),
                     "old element \\(99\\) is not in Container")
        ee(replace(x, old = 99, 0, add = TRUE), container(1, "z", 0))

        x_was_not_touched <- all.equal(x, container(1, "z"))
        expect_true(x_was_not_touched)

        ee(ref_replace(x, 1, 0), container(0, "z"))
        was_changed_by_reference <- isTRUE(all.equal(x, container(0, "z")))
        expect_true(was_changed_by_reference)
    })

    test_that("replace.Set and ref_replace.Set work correctly", {
        x <- setnew(1, "z")
        ee(replace(x, 1, 0), setnew("z", 0))
        ee(replace(x, "z", 0), setnew(1, 0))

        expect_error(replace(x, old = 99, 0), "old element \\(99\\) is not in Set")

        x_was_not_touched <- all.equal(x, setnew(1, "z"))
        expect_true(x_was_not_touched)

        ee(ref_replace(x, 1, 0), setnew("z", 0))
        was_changed_by_reference <- isTRUE(all.equal(x, setnew("z", 0)))
        expect_true(was_changed_by_reference)
    })

    test_that("replace.Deque and ref_replace.Deque work correctly", {
        x <- deque(1, "z")
        ee(replace(x, 1, 0), deque(0, "z"))
        ee(replace(x, "z", 0), deque(1, 0))
        ee(replace(x, "z", NULL), deque(1, NULL))

        expect_error(replace(x, old = 99, 0),
                     "old element \\(99\\) is not in Deque")
        ee(replace(x, old = 99, 0, add = TRUE), deque(1, "z", 0))

        x_was_not_touched <- all.equal(x, deque(1, "z"))
        expect_true(x_was_not_touched)

        ee(ref_replace(x, 1, 0), deque(0, "z"))
        was_changed_by_reference <- isTRUE(all.equal(x, deque(0, "z")))
        expect_true(was_changed_by_reference)
    })

    test_that("replace.Dict and ref_replace.Dict work correctly", {
        d <- dict(a = 1, b = "z")

        ee(replace(d, 1, 0), dict(a = 0, b = "z"))
        ee(replace(d, "z", 0), dict(a = 1, b = 0))
        ee(replace(d, "z", NULL), dict(a = 1, b = NULL))

        expect_error(replace(d, old = 99, 0), "old element \\(99\\) is not in Dict")

        ee(ref_replace(d, 1, 0), dict(a = 0, b = "z"))
        was_changed_by_reference <- isTRUE(all.equal(d, dict(a = 0, b = "z")))
        expect_true(was_changed_by_reference)
    })
})
