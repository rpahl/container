context("Deque S3")

test_that("a Deque object can be created", {
    d <- deque()
    expect_equal(attr(d, "class"), c("Deque", "Container", "Iterable", "R6"))
    expect_true(is.deque(d))
})

test_that("an element can be added to the left of a Deque", {
    expect_equal(values(addleft(deque(0), 3:1)), as.integer(3:0))
    expect_equal(values(addleft(deque(mean), median)), list(median, mean))
})

test_that("number of element occurrences can be counted", {
    names <- deque(c("Lisa", "Bob", "Bob"))
    expect_equal(count(names, "Lisa"), 1)
    expect_equal(count(names, "Bob"), 2)
    expect_equal(count(names, "1"), 0)
    expect_equal(count(add(names, "1"), "1"), 1)
})

test_that("the left and rightmost element can be peeked", {
    v <- 1:3
    d <- deque(v)

    expect_equal(peek(d), utils::tail(v, 1))
    expect_equal(peekleft(d), utils::head(v, 1))
})

test_that("printed Deque object looks as expected", {
    expect_output(print(deque(1:3)),
                  "<Deque> of 3 elements:  int [1:3] 1 2 3", fixed = TRUE)
})

test_that("the left and rightmost element can be popped", {
    v <- 1:3
    d <- deque(v)

    expect_equal(pop(d), utils::tail(v, 1))
    v <- v[-length(v)]
    expect_equal(values(d), v)
    expect_equal(popleft(d), utils::head(v, 1))
    v <- v[-1]
    expect_equal(values(d), v)
})

test_that("popping empty Deques gives an error", {
    expect_error(pop(deque()))
    expect_error(popleft(deque()))
})

test_that("a Deque can be reversed", {
    v <- 1:10
    expect_equal(values(reverse(deque(v))), rev(v))
})

test_that("a Deque can be rotated", {
    d <- deque()
    expect_equal(d, rotate(d))
    add(d, 1)
    expect_equal(values(d), values(rotate(d)))
    expect_equal(values(rotate(deque(1:2))), c(2, 1))
    expect_equal(values(rotate(deque(1:3))), c(3, 1, 2))
    expect_equal(values(rotate(deque(1:3), 2)), c(2, 3, 1))
    expect_equal(values(rotate(deque(1:3), 3)), 1:3)
    expect_equal(values(rotate(deque(1:3), -1)), c(2, 3, 1))
    expect_equal(values(rotate(deque(1:3), -2)), c(3, 1, 2))

    v <- rnorm(10)
    len <- length(v)
    expect_equal(rotate(deque(v), 0), rotate(deque(v), len))
    expect_equal(rotate(deque(v), len), rotate(deque(v), -len))
    expect_equal(rotate(rotate(deque(v), 3), -3), deque(v))
})


test_that("Conversion", {
    d <- deque(as.numeric(1:3))
    expect_equal(deque(1:3), as.deque(1:3))

    expect_equal(as.list(d), as.list(d$values()))
})

test_that("Operators", {
    d <- deque(1:3)
    expect_equal(0 + d, addleft(d, 0))
    expect_equal(d + 0, add(d, 0))
})

