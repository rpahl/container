context("Set S3")

test_that("Set constructor works as expected", {
    s <- setnew()
    expect_true(is.set(s))
    expect_true(empty(s))
    expect_equal(size(s), 0)
    expect_false(has(s, NULL))
    expect_equal(attr(s, "class"), c("Set", "Container", "Iterable", "R6"))

    s <- setnew(1, 1, 1)
    expect_equal(s, setnew(list(1)))
    expect_equal(type(s), "list")
    expect_equal(setnew(mean, mean, 1, 2), setnew(mean, 1, 2))

    s <- setnew(rep(1:4, 2))
    expect_equal(type(s), "numeric")
    expect_equal(values(s), 1:4)
})

test_that("adding special elements works as expected", {
    s <- setnew(NULL)
    expect_equal(size(s), 1)
    add(s, NULL) # cannot be added twice
    expect_equal(size(s), 1)
    expect_equal(values(s), list(NULL))

    add(s, list())
    expect_equal(values(s), list(NULL, list()))
    add(s, list())
    expect_equal(values(s), list(NULL, list()))

    add(s, numeric(0))
    expect_equal(values(s), list(NULL, list(), numeric(0)))

    s <- setnew(0)
    expect_equal(type(s), "numeric")
    expect_error(add(s, NULL), "expected 'numeric' but got 'NULL'")
    s2 <- clone(s)
    expect_equal(add(s, numeric()), s2)
})

test_that("any element can be added to a standard list-type Set", {
    s <- setnew()
    expect_equal(type(s), "list")
    add(s, 1)
    add(s, 2)
    expect_equal(size(s), 2)
    expect_equal(add(s, 2)$size(), 2)
    expect_equal(add(s, list(2))$size(), 3)
    add(s, function(){})
    expect_equal(values(s), list(1, 2, list(2), function(){}))
})

test_that("atomic vectors are added as expected to Sets of atomic types", {
    v <- 1:4
    s <- setnew(1:4)
    expect_equal(type(s), "numeric")
    expect_equal(v, values(s))
    add(s, v)
    expect_equal(v, values(s))

    letters10 <- letters[1:10]
    s <- setnew(letters10)
    expect_equal(type(s), "character")
    expect_equal(values(s), letters10)
    expect_equal(values(add(s, "a")), letters10)
    expect_equal(values(add(s, letters[3:5])), letters10)
})


context("set S3 deprecated")

test_that("set initializer is deprecated and replaced by setnew", {
    s <- expect_warning(set(1, 2, 3))
    expect_equal(s, Set$new(1, 2, 3))
})

