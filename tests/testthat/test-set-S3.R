context("Set S3")

test_that("set", {
    # Initialization and chaining
    expect_true(empty(set()))
    expect_equal(size(set()), 0)
    s1 <- set()
    expect_equal(attr(s1, "name"), "<Set>")
    expect_true(empty(s1))
    expect_equal(size(s1), 0)
    expect_false(has(s1, NULL))
    expect_error(delete(s1, 1), "1 not in Set")

    # add
    add(s1, 1)
    expect_true(has(s1, 1))
    expect_false(empty(s1))
    expect_equal(size(s1), 1)
    expect_equal(size(add(s1, 1)), 1) # should not be added twice
    expect_true(empty(delete(s1, 1)))

    # Lists
    s1 <- set(list(1, 2))
    expect_equal(size(s1), 2)
    expect_equal(size(add(s1, 2)), 2)
    expect_equal(size(add(s1, list(2))), 3)
    expect_equal(values(s1), list(1, 2, list(2)))

    # Vectors
    expect_equal(values(set(rep(1, 10))), 1)
    letters10 <- letters[1:10]
    s1 <- set(letters10)
    expect_equal(values(s1), letters10)
    expect_equal(values(add(s1, "a")), letters10)
    expect_equal(values(add(s1, letters[3:5])), letters10)

    # set operations
    l1 <- list(1, 2, 3,    "A", "B", "C")
    l2 <- list(   2, 3, 4,      "B", "C", "D")
    s1 <- set(l1)
    s2 <- set(l2)
    expect_equal(values(s1 + s2), union(l1, l2))
    expect_true(setequal(values(s1 + s2), values(s2 + s1)))
    expect_equal(values(s1 / s2), intersect(l1, l2))
    expect_true(setequal(values(s1 / s2), values(s2 / s1)))
    expect_equal(values(s1 - s2), setdiff(l1, l2))
    expect_false(setequal(values(s1 - s2), values(s2 - s1)))
    expect_equal(values(s2 - s1), setdiff(l2, l1))
    expect_false(s1 < s2)
    expect_false(s1 > s2)
    expect_false(s2 < s1)
    expect_false(s2 > s1)
    expect_true((s1 + s2) > s1)
    expect_true((s1 + s2) > s2)
    expect_true((s1 / s2) < s1)
    expect_true((s1 - s2) < s1)
})

test_that("S3 methods", {
    expect_equal(set(), Set$new())
    expect_true(is.set(set()))
    expect_equal(set(1:3), as.set(1:3))
    s1 <- set(1:3)
    s2 <- set(3:5)
    expect_equal(s1 + s2, s1$union(s2))
    expect_equal(s1 / s2, s1$intersect(s2))
    expect_equal(s1 - s2, s1$diff(s2))

    expect_output(print(s1), "<Set> of 3 elements:  int \\[1:3\\] 1 2 3")
    expect_equal(as.list(s1), as.list(s1$values()))
    expect_equal(as.vector(s1), s1$values())
})

