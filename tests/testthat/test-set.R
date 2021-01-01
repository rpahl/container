context("Set")

test_that("Set constructor works as expected", {
    s <- Set$new()
    expect_true(s$empty())
    expect_equal(s$size(), 0)
    expect_false(s$has(NULL))
    expect_equal(attr(s, "class"), c("Set", "Container", "Iterable", "R6"))

    s <- Set$new(1, 1, 1)
    expect_equal(s, Set$new(list(1)))
    expect_equal(s$type(), "list")
    expect_equal(Set$new(mean, mean, 1, 2), Set$new(mean, 1, 2))

    s <- Set$new(rep(1:4, 2))
    expect_equal(s$type(), "numeric")
    expect_equal(s$values(), 1:4)
})

test_that("adding special elements works as expected", {
    s <- Set$new(NULL)
    expect_equal(s$size(), 1)
    s$add(NULL) # cannot be added twice
    expect_equal(s$size(), 1)
    expect_equal(s$values(), list(NULL))

    s$add(list())
    expect_equal(s$values(), list(NULL, list()))
    s$add(list())
    expect_equal(s$values(), list(NULL, list()))

    s$add(numeric(0))
    expect_equal(s$values(), list(NULL, list(), numeric(0)))

    s <- Set$new(0)
    expect_equal(s$type(), "numeric")
    expect_error(s$add(NULL), "expected 'numeric' but got 'NULL'")
    s2 <- s$clone()
    expect_equal(s$add(numeric()), s2)
})

test_that("any element can be added to a standard list-type Set", {
    s <- Set$new()
    expect_equal(s$type(), "list")
    s$add(1)$add(2)
    expect_equal(s$size(), 2)
    expect_equal(s$add(2)$size(), 2)
    expect_equal(s$add(list(2))$size(), 3)
    s$add(function(){})
    expect_equal(s$values(), list(1, 2, list(2), function(){}))
})

test_that("atomic vectors are added as expected to Sets of atomic types", {
    v <- 1:4
    s <- Set$new(1:4)
    expect_equal(s$type(), "numeric")
    expect_equal(v, s$values())
    s$add(v)
    expect_equal(v, s$values())

    letters10 <- letters[1:10]
    s <- Set$new(letters10)
    expect_equal(s$type(), "character")
    expect_equal(s$values(), letters10)
    expect_equal(s$add("a")$values(), letters10)
    expect_equal(s$add(letters[3:5])$values(), letters10)
})

