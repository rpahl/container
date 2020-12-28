context("Dict")

test_that("Dict constructor works as expected", {
    expect_true(Dict$new()$empty())
    expect_equal(Dict$new()$type(), "list")
    expect_error(Dict$new(1:2), "all elems must be named")
    expect_error(Dict$new(x = 1, y = 2, x = 3), "duplicated keys")
    expect_equal(Dict$new()$keys(), character(0))
    expect_equal(Dict$new(c(x = 1, y = 2))$type(), "numeric")
    expect_equal(Dict$new(list(x = 1, y = 2))$type(), "list")

    # One element
    expect_equal(Dict$new(x = 1)$values(), list(x = 1))
    expect_equal(Dict$new(x = 1), Dict$new(list(x = 1)))
    expect_equal(Dict$new(c(x = 1))$values(), c(x = 1))
    expect_equal(Dict$new(c(x = c(1, 4)))$values(), c(x1 = 1, x2 = 4))
    expect_equal(Dict$new(list(x = c(1, 4)))$values(), list(x = c(1, 4)))

    # Two (or more) elements
    d <- Dict$new(x = 1:2, y = 2:3)
    expect_true(inherits(d, "Dict"))
    expect_true(inherits(d, "Container"))
    expect_equal(d, Dict$new(list(x = 1:2, y = 2:3)))
    expect_equal(d$values(),
                 Container$new(x = 1:2, y = 2:3, keep_names = TRUE)$values())
    expect_equal(names(d$values()), c("x", "y"))

    expect_equal(Dict$new(env = environment(), id = identity)$size(), 2)
})

test_that("adding elements to a Dict required a character key and a value", {
    d <- Dict$new()
    expect_error(d$add(1), "key must be character")
    expect_error(d$add("", 1, "zero-length key"))
    expect_error(d$add("a"), 'argument "value" is missing')
    expect_error(d$add(value = 1), 'argument "key" is missing')
    expect_error(d$add(c("a", "b"), 1:2), 'key must be of length 1')
    expect_error(d$add(1, 1), 'key must be character')
})

test_that("added elements must have distinct keys and cannot be added twice", {
    d <- Dict$new()
    d$add("a", 1)
    expect_equal(d$values(), list(a = 1))
    expect_error(d$add("a", 1), "key 'a' already in Dict")
    expect_error(d$add("a", 2), "key 'a' already in Dict")
    expect_error(d$add("a", NULL), "key 'a' already in Dict")
})

test_that("NULL and empty lists can be added", {
    d <- Dict$new()
    d$add("null", NULL)
    d$add("empty-list", list())

    expect_equal(d$values(), list("null" = NULL, "empty-list" = list()))
})

test_that("the underlying type changes depending on added elements as with base R", {
    d <- Dict$new(c(x = 1))
    expect_equal(d$type(), "numeric")
    d$add("y", "a")
    expect_equal(d$type(), "character")
    d$add("n", NULL)
    expect_equal(d$type(), "list")
})

test_that("elements can be deleted from a Dict", {
    d <- Dict$new(a = 1)
    expect_false(d$empty())
    expect_true(d$delete("a")$empty())
})

test_that("if key not in Dict, trying to delete it gives an error", {
    expect_error(Dict$new(a = 1)$delete("b"), "key 'b' not in Dict")
})

test_that("only one key can be deleted at a time", {
    d <- Dict$new(a = 1, b = 2)
    expect_error(d$delete(c("a", "b"), "key must be of length 1"),
                 "key must be of length 1")
    d_was_not_touched <- d$size() == 2
    expect_true(d_was_not_touched)
})

test_that("elements can be discarded", {
    d <- Dict$new(a = 1)
    expect_false(d$empty())
    expect_true(d$discard("a")$empty())
})

test_that("discard ignores non-existing elements without error", {
    d <- Dict$new(a = 1)
    expect_equal(d$values(), list(a = 1))
    d$discard("b")
    expect_equal(d$values(), list(a = 1))
})

test_that("only one key can be discarded at a time", {
    d <- Dict$new(a = 1, b = 2)
    expect_error(d$discard(c("a", "b"), "key must be of length 1"),
                 "key must be of length 1")
    d_was_not_touched <- d$size() == 2
    expect_true(d_was_not_touched)
})

test_that("getting an element throws an error if key does not exist", {
    d <- Dict$new(a = 1, b = 2, n = NULL)
    expect_equal(d$getval("a"), 1)
    expect_true(is.null(d$getval("n")))
    expect_error(d$getval("x"), "key 'x' not in Dict")
})

test_that("only one element at a time can be get", {
    d <- Dict$new(a = 1, b = 2)
    expect_error(d$getval(c("a", "b")), "key must be of length 1")
})

test_that("it can be checked if Dict has a certain key", {
    d <- Dict$new(a = 1, b = 2)
    expect_true(d$has("a"))
    expect_false(d$has("x"))
    expect_error(d$has(c("a", "b")), "key must be of length 1")
})

test_that("all keys can be listed", {
    d <- Dict$new(a = 1, b = 2)
    expect_equal(d$keys(), c("a", "b"))
    d$delete("a")
    expect_equal(d$keys(), "b")
})

test_that("elements can be peeked and return default value if key does not exist", {
    d <- Dict$new(a = 1, b = 2)
    expect_equal(d$peek("a"), d$getval("a"))
    expect_true(is.null(d$peek("x")))

    expect_equal(d$peek("x", default = 9), 9)
})

test_that("elements can be popped and popping non-existent element gives an error ", {
    x <- c(a = 1, b = 2)
    d <- Dict$new(x)
    expect_equal(d$pop("a"), x[["a"]])
    expect_false(d$has("a"))
    expect_error(d$pop("a"))
})

test_that("elements can be popped randomly from Dict", {
    x <- c(a = 1, b = 2)
    d <- Dict$new(x)
    v <- numeric(0)
    while (!d$empty()) {
        v <- c(v, d$popitem())
    }
    expect_equal(sort(v), as.numeric(x))

    expect_true(d$empty())
    expect_error(d$popitem(), "pop at empty Dict")
})

test_that("A key in the Dict can be renamed", {
    d <- Dict$new(list(A = 1, B = 2))
    expect_error(d$rename(1, "C"), "key must be character")
    expect_error(d$rename("A", 1), "key must be character")
    expect_error(d$rename("A", c("C", "D")), "must be of same length")
    expect_error(d$rename("A", "B"), "rename failed because 'B' exists already")
    expect_error(d$rename("Z", "B"), "key 'Z' not found")

    vals = as.numeric(d$values())
    d$rename("A", "a")
    expect_true(d$has("a"))
    expect_false(d$has("A"))

    # Verify that values did not change
    expect_equal(vals, as.numeric(d$values()))

    # Several keys at once
    d$rename(c("a", "B"), c("x", "y"))
    expect_equal(d$keys(), c("x", "y"))

    # Renaming same key multiple times is possible
    d$rename(c("x", "x2"), c("x2", "x3"))
    expect_equal(d$keys(), c("x3", "y"))
})

test_that("an existing value can be changed in a Dict", {
    d <- Dict$new(a = 1, b = NULL)
    d$setval("b", list(1, 2))
    expect_equal(d$getval("b"), list(1, 2))
    expect_error(d$setval("x", 1), "key 'x' not in Dict")
})

test_that("a Dict can be re-sorted according to its keys", {
    d <- Dict$new()
    d$add("b", 1)$add("a", 2)
    expect_equal(d$keys(), c("b", "a"))
    expect_equal(d$sortkey()$keys(), c("a", "b"))
})

test_that("a Dict can be updated by another Dict object", {
    d1 <- Dict$new(list(A = 1, B = 2, C = 12))
    d2 <- Dict$new(list(              C = 3, D = 4))
    expect_error(d1$update(list()), "arg must be a Dict")
    expect_equal(d1$update(Dict$new()), d1)
    expect_equal(d1$update(d2)$values(), list(A = 1, B = 2, C = 3, D = 4))
    expect_equal(Dict$new()$update(d2), d2)
})

context("dict deprecated")

test_that("Dict get is deprecated and replaced by getval", {
    d <- Dict$new(a = 1, b = 2)
    expect_warning(expect_equal(d$get("b"), 2), "Use 'getval' instead.")
})

test_that("Dict set is deprecated and replaced by setval", {
    d <- Dict$new(a = 1, b = 2)
    expect_warning(d$set("b", 9), "Use 'setval' instead.")
    expect_equal(d$peek("b"), 9)
})

test_that("Dict remove is deprecated and replaced by delete", {
    d <- Dict$new(a = 1, b = 2)
    expect_warning(d$remove("b"), "Use 'delete' instead.")
    expect_false(d$has("b"))
})

test_that("Dict sort is deprecated and replaced by sortkey", {
    d <- Dict$new(a = 1, b = 2)
    expect_warning(d$remove("b"), "Use 'delete' instead.")
    expect_false(d$has("b"))
})

