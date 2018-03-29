context("Set")

test_that("Set", {
    # Initialization and chaining
    expect_true(Set$new()$empty())
    expect_equal(Set$new()$size(), 0)
    s1 <- Set$new()
    expect_true(s1$empty())
    expect_equal(s1$size(), 0)
    expect_false(s1$has(NULL))
    expect_error(s1$remove(1), "1 not in Set")

    # add
    s1$add(1)
    expect_true(s1$has(1))
    expect_false(s1$empty())
    expect_equal(s1$size(), 1)
    expect_equal(s1$add(1)$size(), 1) # should not be added twice
    expect_true(s1$remove(1)$empty())

    # Lists
    s1 <- Set$new(list(1, 2))
    expect_equal(s1$size(), 2)
    expect_equal(s1$add(2)$size(), 2)
    expect_equal(s1$add(list(2))$size(), 3)
    expect_equal(s1$values(), list(1, 2, list(2)))

    # Vectors
    expect_equal(Set$new(rep(1, 10))$values(), 1)
    letters10 <- letters[1:10]
    s1 <- Set$new(letters10)
    expect_equal(s1$values(), letters10)
    expect_equal(s1$add("a")$values(), letters10)
    expect_equal(s1$add(letters[3:5])$values(), letters10)

    # set operations
    l1 <- list(1, 2, 3,    "A", "B", "C")
    l2 <- list(   2, 3, 4,      "B", "C", "D")
    s1 <- Set$new(l1)
    s2 <- Set$new(l2)
    expect_equal(s1$union(s2)$values(), union(l1, l2))
    expect_true(setequal(s1$union(s2)$values(), s2$union(s1)$values()))
    expect_equal(s1$intersect(s2)$values(), intersect(l1, l2))
    expect_true(setequal(s1$intersect(s2)$values(), s2$intersect(s1)$values()))
    expect_equal(s1$diff(s2)$values(), setdiff(l1, l2))
    expect_false(setequal(s1$diff(s2)$values(), s2$diff(s1)$values()))
    expect_equal(s2$diff(s1)$values(), setdiff(l2, l1))
    expect_false(s1$is.subset(s2))
    expect_false(s1$is.superset(s2))
    expect_false(s2$is.subset(s1))
    expect_false(s2$is.superset(s1))
    expect_true(s1$union(s2)$is.superset(s1))
    expect_true(s1$union(s2)$is.superset(s2))
    expect_true(s1$intersect(s2)$is.subset(s1))
    expect_true(s1$diff(s2)$is.subset(s1))
})

