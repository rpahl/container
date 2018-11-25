context("Deque S3")

test_that("deque", {
    # addleft
    d <- deque(1L)
    expect_equal(attr(d, "name"), "<Deque>")
    expect_equal(type(d), "integer")
    addleft(d, 2.2)
    expect_equal(type(d), "integer")
    expect_warning(add(d, "a"), "NAs introduced by coercion")
    expect_equal(values(3:1 + deque(0L)), as.integer(3:0))

    # count
    names <- deque(c("Lisa", "Bob", "Bob"))
    expect_equal(type(names), "character")
    expect_equal(count(names, "Lisa"), 1)
    expect_equal(count(names, "Bob"), 2)
    expect_equal(count(names, "1"), 0)
    expect_equal(count(add(names, 1), "1"), 1)

    # peek and pop
    v <- 1:3
    d <- deque(v)
    expect_output(print(d), " int [1:3] 1 2 3", fixed=TRUE)
    expect_equal(type(d), "integer")
    expect_equal(size(d), length(v))
    expect_equal(peek(d), tail(v, 1))
    expect_equal(pop(d), tail(v, 1))
    v <- v[-length(v)] # emulate pop
    expect_equal(values(d), v)
    expect_equal(pop(d), 2)
    expect_equal(pop(d), 1)
    expect_error(pop(d), "pop at empty Deque")

    # peekleft and popleft
    v <- 1:3
    d <- deque(v)
    expect_equal(peekleft(d), head(v, 1))
    expect_equal(popleft(d), head(v, 1))
    v <- v[-1] # emulate popleft
    expect_equal(values(d), v)
    expect_equal(popleft(d), 2)
    expect_equal(popleft(d), 3)
    expect_error(popleft(d), "popleft at empty Deque")

    # reverse
    v <- 1:10
    expect_equal(values(reverse(deque(v))), rev(v))

    # rotate
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
    expect_true(is.deque(deque()))
    d <- deque(as.numeric(1:3))
    expect_equal(deque(1:3), as.deque(1:3))

    expect_output(print(d), "<Deque> of 3 elements:  num \\[1:3\\] 1 2 3")
    expect_equal(as.list(d), as.list(d$values()))
    expect_equal(as.vector(d), d$values())
})

test_that("Operators", {
    d <- deque(1:3)
    expect_equal(0 + d, addleft(d, 0))
    expect_equal(d + 0, add(d, 0))
})
