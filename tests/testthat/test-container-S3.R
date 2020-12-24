context("container S3")

test_that("Container initialization works as expected", {
    co <- container()
    expect_true(is.container(co))
    expect_equal(attr(co, "class"), c("Container", "Iterable", "R6"))

    co <- container(1:4)
    expect_equal(type(co), "numeric")

    co <- container(environment())
    expect_equal(type(co), "list")
    expect_equal(size(co), 1)

    co <- container(environment(), foo = identity)
    expect_equal(size(co), 2)

    co <- container(A = 1, B = 2)
    expect_equal(type(co), "list")
    expect_true(is.null(names(values(co))))
})

test_that("type of Container is inialized as expected", {
    expect_equal(type(container()), "list")
    expect_equal(type(container(1)), "numeric")
    expect_equal(type(container(new.env())), "list")
    expect_equal(type(container(TRUE)), "logical")
    expect_equal(type(container(function(){})), "list")
    expect_equal(type(container(raw())), "raw")
    expect_equal(type(container(0+0i)), "complex")
    expect_equal(type(container(letters[1:10])), "character")
})

test_that("it can be checked whether the Container is empty", {
    expect_true(empty(container()))
    expect_true(empty(container(numeric())))
    expect_false(empty(container(1)))
})

test_that("elements can be added to the Container", {
    co <- container()
    expect_true(empty(co))
    add(co, 1)
    expect_equal(values(co), list(1))

    co <- container(numeric())
    add(co, 1)
    expect_equal(values(co), 1)
})

test_that("NULL and empty lists can be added to a Container", {
    co <- container()
    add(co, NULL)
    add(co, list())
    add(co, 0)
    add(co, NULL)
    add(co, list())

    expect_equal(size(co), 5)
    delete(co, NULL)
    delete(co, list())
    expect_equal(size(co), 3)
    delete(co, list())
    delete(co, NULL)
    expect_equal(values(co), list(0))

    expect_true(empty(add(container(numeric(0)), numeric(0))))
})

test_that("types of added elements must match for non-list Containers", {
    co <- container(1)
    expect_equal(values(co), 1)
    add(co, 2)
    expect_equal(values(co), 1:2)
    expect_error(add(co, "a"), "type mismatch: expected 'numeric' but got 'character'")
    expect_error(add(co, list(1)), "type mismatch: expected 'numeric' but got 'list'")
    expect_error(add(co, list()), "type mismatch: expected 'numeric' but got 'list'")
    expect_error(add(co, NULL), "type mismatch: expected 'numeric' but got 'NULL'")
    expect_equal(values(add(co, 3:5)), 1:5)
})

test_that("non-trivial objects are added correctly", {
    v <- 1:10
    env <- new.env()
    ll <- list(1, 2, "A")
    foo <- function() print("foo")
    collection <- c(list(v), list(env), list(ll), list(foo))

    co <- container()
    for (elem in collection) add(co, elem)
    expect_equal(values(co), collection)
    expect_equal(size(co), length(collection))
})

test_that("a Container can be added to a Container", {
    v <- 1:10
    co <- container(v)
    coco <- container()
    add(coco, co)
    expect_equal(values(coco)[[1]], co)
    expect_equal(values(values(coco)[[1]]), v)
})

test_that("a cleared Container preserves its type", {
    expect_equal(type(clear(container())), "list")
    expect_equal(type(clear(container(1:3))), "numeric")
    expect_equal(type(clear(container("a"))), "character")
})

test_that("it can be determined whether Container contains a certain element", {
    co <- container(1:5)
    expect_true(has(co, 1))
    expect_false(has(co, 7))
    expect_true(has(add(co, 7), 7))

    foo <- function() print("foo")
    co <- container(mean, foo, identity)
    expect_true(has(co, identity))
    expect_true(has(co, mean))
    expect_false(has(co, median))
    expect_true(has(co, function() print("foo")))
    expect_false(has(co, function() print("bar")))
})

test_that("elements can be discarded from a Container", {
    x <- 1:5
    co <- container(x)
    discard(co, 3)
    expect_equal(values(co), x[-3])

    co <- container(mean, identity)
    expect_equal(values(discard(co, mean)), list(identity))

    expect_error(discard(co), 'argument "elem" is missing, with no default')
})

test_that("elements can be discarded from left and from right", {
    co <- container(c(1, 2, 1))
    expect_equal(values(discard(co, 1)), 2:1)

    co <- container(c(1, 2, 1))
    expect_equal(values(discard(co, 1, right = TRUE)), 1:2)
})

test_that("discarding non-existent elements does not change Container", {
    co <- container(1:3)
    expect_equal(values(discard(co, 5)), 1:3)
    expect_equal(discard(container()), container())
})

test_that("elements can be deleted from a Container", {
    x <- 1:5
    co <- container(x)
    delete(co, 3)
    expect_equal(values(co), x[-3])

    co <- container(mean, identity)
    expect_equal(values(delete(co, mean)), list(identity))
    expect_error(delete(co), 'argument "elem" is missing, with no default')
})

test_that("Container gives an error if trying to delete non-existing element", {
    co <- container(1:3)
    expect_error(delete(co, 5), "5 not in Container")
})

test_that("elements can be deleted from left and from right", {
    co <- container(c(1, 2, 1))
    expect_equal(values(delete(co, 1)), 2:1)

    co <- container(c(1, 2, 1))
    expect_equal(values(delete(co, 1, right = TRUE)), 1:2)
})

test_that("the size of a Container can be retrieved", {
    expect_equal(container()$size(), 0)
    x <- 1:5
    co <- container(x)
    expect_equal(size(co), length(x))

    ll <- list(mean, identity)
    co <- container(ll)
    expect_equal(size(co), length(ll))
})

test_that("the data values of a Container can be retrieved", {
    expect_equal(values(container()), list())
    expect_equal(values(container(1:5)), 1:5)
})

test_that("Container objects provide reference semantics but can also be cloned", {
    c1 <- container(1:10)
    c2 <- c1
    cc <- clone(c1)
    expect_true(identical(c1, c2))
    expect_false(identical(c1, cc))
    expect_equal(size(c1), size(cc))
    expect_equal(c1, cc)

    delete(c1, 7)
    expect_true(identical(c1, c2))
    expect_lt(size(c1), size(cc))
})


test_that("Iterator can be constructed from Container", {
    v <- 1:5
    co <- container(v)
    it <- iter(co)
    sum <- 0
    while(has_next(it)) sum <- sum + get_next(it)
    expect_equal(sum(v), sum(values(co)))
})

test_that("Container object can be converted to base list", {
    expect_equal(as.list(container(1:5)), as.list(1:5))
    expect_equal(as.list(container("A", mean, globalenv())),
                 list("A", mean, globalenv()))
})

