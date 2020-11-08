context("Dict")

test_that("Dict constructor works as expected", {
    # initialize
    expect_error(Dict$new(1:2), "all items must be named")
    expect_equal(Dict$new()$keys(), character(0))
    d <- Dict$new(c(x=1L, y=2L))
    expect_is(d, "Dict")
    expect_is(d, "Container")
    expect_error(d$get("z"), "key 'z' not in Dict")
    expect_error(d$add(key="", 3L, "zero-length key"))
    expect_equal(as.integer(d$values()), 1:2)
    expect_equal(d$type(), "integer")
    expect_equal(Dict$new()$type(), "list")
    expect_error(Dict$new(list(x=1, y=2, x=3)), "duplicated keys")
})

test_that("Dict operations work as expected", {
    # empty, size, has, add and peek
    d <- Dict$new()
    expect_equal(attr(d, "name"), "<Dict>")
    expect_true(d$empty())
    expect_error(d$add(key=1, 1), "key must be character")
    expect_error(d$add(c("x", "y"), 1), "key must be of length 1")
    d$add("x", 1)
    expect_false(d$empty())
    expect_equal(d$size(), 1)
    expect_true(d$has("x"))
    expect_equal(d$peek("x"), 1)
    expect_equal(d$peek("foo"), NULL)
    expect_equal(d$peek("foo", default=0), 0)
    expect_error(d$add("x", 2), "key 'x' already in Dict")

    # set and pop
    d$set("x", 2)$set("x", 3)
    expect_equal(d$size(), 1)
    expect_equal(d$peek("x"), 3)
    expect_equal(d$pop("x"), 3)
    expect_false(d$has("x"))

    # keys, discard, remove, popitem
    d <- Dict$new(integer())$add("x", 1)$add("y", 2)$add("z", 3)
    expect_output(print(d), 'Named num [1:3] 1 2 3', fixed=TRUE)
    expect_true(d$has("y"))
    expect_equal(d$keys(), c("x", "y", "z"))
    expect_false(d$discard("y")$has("y"))
    expect_error(d$remove("y"), "key 'y' not in Dict")
    expect_false(d$discard("y")$has("y")) # no error although not in Dict
    expect_error(d$set("y", 10), "key 'y' not in Dict")
    expect_true(d$set("y", 10, add=TRUE)$has("y"))

    v <- d$values() # x=1, z=3, y=10
    v2 <- vector(mode = d$type())
    set.seed(123)
    for(i in seq_len(d$size())) {
        v2 <- c(v2, d$popitem())
    }
    expect_true(d$empty())
    expect_error(d$popitem(), "pop at empty Dict")
    expect_true(setequal(v, v2))
    expect_equal(names(sort(v)), names(sort(v2)))
})


test_that("Dict sort", {
    d <- Dict$new()
    d$add("b", 1)$add("a", 2)
    expect_equal(d$keys(), c("b", "a"))
    expect_equal(d$sort()$keys(), c("a", "b"))
})

test_that("Dict update", {
    d1 <- Dict$new(list(A=1, B=2, C=12))
    expect_error(d1$update(list()), "arg must be a Dict")
    d2 <- Dict$new(list(          C=3, D=4))
    expect_equal(d1$update(Dict$new()), d1)
    expect_equal(d1$update(d2)$values(), list(A=1, B=2, C=3, D=4))
    expect_equal(Dict$new()$update(d2), d2)
})


