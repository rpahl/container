context("Container")

test_that("Container constructor works as expected", {
    co <- Container$new()
    expect_equal(attr(co, "class"), c("Container", "Iterable", "R6"))

    co <- Container$new(1:4)
    expect_equal(mode(co$values()), "numeric")

    co <- Container$new(environment())
    expect_equal(mode(co$values()), "list")
    expect_equal(co$size(), 1)

    co <- Container$new(environment(), foo = identity)
    expect_equal(co$size(), 2)

    co <- Container$new(A = 1, B = 2)
    expect_true(is.null(names(co$values())))
    co <- Container$new(A = 1, B = 2, keep_names = TRUE)
    expect_equal(names(co$values()), c("A", "B"))

    expect_equal(Container$new(keep_names = TRUE),
                 Container$new(keep_names = FALSE))
})

test_that("type of Container is inialized as expected", {
    expect_equal(mode(Container$new()$values()), "list")
    expect_equal(mode(Container$new(1)$values()), "numeric")
    expect_equal(mode(Container$new(new.env())$values()), "list")
    expect_equal(mode(Container$new(TRUE)$values()), "logical")
    expect_equal(mode(Container$new(function(){})$values()), "list")
    expect_equal(mode(Container$new(raw())$values()), "raw")
    expect_equal(mode(Container$new(0+0i)$values()), "complex")
    expect_equal(mode(Container$new(letters[1:10])$values()), "character")
})

test_that("it can be checked whether the Container is empty", {
    expect_true(Container$new()$empty())
    expect_true(Container$new(numeric())$empty())
    expect_false(Container$new(1)$empty())
})

test_that("elements can be added to the Container", {
    co <- Container$new()
    expect_true(co$empty())
    co$add(1)
    expect_equal(co$values(), list(1))

    co <- Container$new(numeric())
    co$add(1)
    expect_equal(co$values(), 1)
})

test_that("NULL and empty lists can be added to a Container", {
    co <- Container$new()
    co$add(NULL)
    co$add(list())
    co$add(0)
    co$add(NULL)
    co$add(list())

    expect_equal(co$size(), 5)
    co$delete(NULL)
    co$delete(list())
    expect_equal(co$size(), 3)
    co$delete(list())
    co$delete(NULL)
    expect_equal(co$values(), list(0))

    expect_true(Container$new(numeric(0))$add(numeric(0))$empty())
})

test_that("types of added elements must match for non-list Containers", {
    co <- Container$new(1)
    expect_equal(co$values(), 1)
    co$add(2)
    expect_equal(co$values(), 1:2)
    expect_error(co$add("a"), "type mismatch: expected 'numeric' but got 'character'")
    expect_error(co$add(list(1)), "type mismatch: expected 'numeric' but got 'list'")
    expect_error(co$add(list()), "type mismatch: expected 'numeric' but got 'list'")
    expect_error(co$add(NULL), "type mismatch: expected 'numeric' but got 'NULL'")
    expect_equal(co$add(3:5)$values(), 1:5)
})

test_that("non-trivial objects are added correctly", {
    v <- 1:10
    env <- new.env()
    ll <- list(1, 2, "A")
    foo <- function() print("foo")
    collection <- c(list(v), list(env), list(ll), list(foo))

    co <- Container$new()
    co$add(v)$add(env)$add(ll)$add(foo)
    expect_equal(co$values(), collection)
    expect_equal(co$size(), length(collection))
})

test_that("a Container can be added to a Container", {
    v <- 1:10
    co <- Container$new(v)
    coco <- Container$new()
    coco$add(co)
    expect_equal(coco$values()[[1]], co)
    expect_equal(coco$values()[[1]]$values(), v)
})

test_that("named elements can be added to a Container", {
    co <- Container$new(numeric())
    x <- 1
    names(x) <- "x"
    co$add(x)

    y <- 1:3
    names(y) <- letters[1:3]
    co$add(y)

    expect_equal(co$values(), c(x, y))
})


test_that("a cleared Container preserves its type", {
    expect_equal(mode(Container$new()$clear()$values()), "list")
    expect_equal(mode(Container$new(1:3)$clear()$values()), "numeric")
    expect_equal(mode(Container$new("a")$clear()$values()), "character")
})

test_that("it can be determined whether Container contains a certain element", {
    co <- Container$new(1:5)
    expect_true(co$has(1))
    expect_false(co$has(7))
    expect_true(co$add(7)$has(7))

    foo <- function() print("foo")
    co <- Container$new(mean, foo, identity)
    expect_true(co$has(identity))
    expect_true(co$has(mean))
    expect_false(co$has(median))
    expect_true(co$has(function() print("foo")))
    expect_false(co$has(function() print("bar")))
})

test_that("elements can be discarded from a Container", {
    x <- 1:5
    co <- Container$new(x)
    co$discard(3)
    expect_equal(co$values(), x[-3])

    co <- Container$new(mean, identity)
    expect_equal(co$discard(mean)$values(), list(identity))

    expect_error(co$discard(), 'argument "elem" is missing, with no default')
})

test_that("elements can be discarded from left and from right", {
    co <- Container$new(c(1, 2, 1))
    expect_equal(co$discard(1)$values(), 2:1)

    co <- Container$new(c(1, 2, 1))
    expect_equal(co$discard(1, right = TRUE)$values(), 1:2)
})

test_that("discarding non-existent elements does not change Container", {
    co <- Container$new(1:3)
    expect_equal(co$discard(5)$values(), 1:3)
    expect_equal(Container$new()$discard(1), Container$new())
})

test_that("elements can be deleted from a Container", {
    x <- 1:5
    co <- Container$new(x)
    co$delete(3)
    expect_equal(co$values(), x[-3])

    co <- Container$new(mean, identity)
    expect_equal(co$delete(mean)$values(), list(identity))
    expect_error(co$delete(), 'argument "elem" is missing, with no default')
})

test_that("Container gives an error if trying to delete non-existing element", {
    co <- Container$new(1:3)
    expect_error(co$delete(5), "5 not in Container")
})

test_that("elements can be deleted from left and from right", {
    co <- Container$new(c(1, 2, 1))
    expect_equal(co$delete(1)$values(), 2:1)

    co <- Container$new(c(1, 2, 1))
    expect_equal(co$delete(1, right = TRUE)$values(), 1:2)
})

test_that("the size of a Container can be retrieved", {
    expect_equal(Container$new()$size(), 0)
    x <- 1:5
    co <- Container$new(x)
    expect_equal(co$size(), length(x))

    ll <- list(mean, identity)
    co <- Container$new(ll)
    expect_equal(co$size(), length(ll))
})

test_that("the data values of a Container can be retrieved", {
    expect_equal(Container$new()$values(), list())
    expect_equal(Container$new(1:5)$values(), 1:5)
})

test_that("Container objects provide reference semantics but can also be cloned", {
    c1 <- Container$new(1:10)
    c2 <- c1
    cc <- c1$clone()
    expect_true(identical(c1, c2))
    expect_false(identical(c1, cc))
    expect_equal(c1$size(), cc$size())
    expect_equal(c1, cc)

    c1$delete(7)
    expect_true(identical(c1, c2))
    expect_lt(c1$size(), cc$size())
})


test_that("Iterator can be constructed from Container", {
    v <- 1:5
    co <- Container$new(v)
    it <- co$iter()
    sum <- 0
    while(it$has_next()) sum <- sum + it$get_next()
    expect_equal(sum(v), sum(co$values()))
})


test_that("type is deprecated", {
    co <- Container$new(1:10)
    expect_warning(expect_equal(co$type(), "numeric"),
                   "'co$type()' is deprecated.", fixed = TRUE)
})

