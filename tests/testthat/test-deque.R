context("Deque")

test_that("Deque", {
    # addleft
    d <- Deque$new(1L)
    expect_equal(attr(d, "name"), "<Deque>")
    expect_equal(d$type(), "integer")
    d$addleft(2.2)
    expect_equal(d$type(), "integer")
    expect_warning(d$add("a"), "NAs introduced by coercion")
    expect_equal(Deque$new(0L)$addleft(3:1)$values(), as.integer(3:0))

    # count
    names <- Deque$new(c("Lisa", "Bob", "Bob"))
    expect_equal(names$type(), "character")
    expect_equal(names$count("Lisa"), 1)
    expect_equal(names$count("Bob"), 2)
    expect_equal(names$count("1"), 0)
    expect_equal(names$add(1)$count("1"), 1)

    # peek and pop
    v <- 1:3
    d <- Deque$new(v)
    expect_output(print(d), " int [1:3] 1 2 3", fixed=TRUE)
    expect_equal(d$type(), "integer")
    expect_equal(d$size(), length(v))
    expect_equal(d$peek(), tail(v, 1))
    expect_equal(d$pop(), tail(v, 1))
    v <- v[-length(v)] # emulate pop
    expect_equal(d$values(), v)
    expect_equal(d$pop(), 2)
    expect_equal(d$pop(), 1)
    expect_error(d$pop(), "pop at empty Deque")

    # peekleft and popleft
    v <- 1:3
    d <- Deque$new(v)
    expect_equal(d$peekleft(), head(v, 1))
    expect_equal(d$popleft(), head(v, 1))
    v <- v[-1] # emulate popleft
    expect_equal(d$values(), v)
    expect_equal(d$popleft(), 2)
    expect_equal(d$popleft(), 3)
    expect_error(d$popleft(), "popleft at empty Deque")

    # reverse
    v <- 1:10
    expect_equal(Deque$new(v)$reverse()$values(), rev(v))

    # rotate
    d <- Deque$new()
    expect_equal(d, d$rotate())
    d$add(1)
    expect_equal(d$values(), d$rotate()$values())
    expect_equal(Deque$new(1:2)$rotate()$values(), c(2, 1))
    expect_equal(Deque$new(1:3)$rotate()$values(), c(3, 1, 2))
    expect_equal(Deque$new(1:3)$rotate(2)$values(), c(2, 3, 1))
    expect_equal(Deque$new(1:3)$rotate(3)$values(), 1:3)
    expect_equal(Deque$new(1:3)$rotate(-1)$values(), c(2, 3, 1))
    expect_equal(Deque$new(1:3)$rotate(-2)$values(), c(3, 1, 2))
    v <- rnorm(10)
    len <- length(v)
    expect_equal(Deque$new(v)$rotate(0), Deque$new(v)$rotate(len))
    expect_equal(Deque$new(v)$rotate(len), Deque$new(v)$rotate(-len))
    expect_equal(Deque$new(v)$rotate(3)$rotate(-3), Deque$new(v))
})

test_that("S3 methods", {
    expect_equal(deque(), Deque$new())
    expect_true(is.deque(deque()))
    d <- deque(as.numeric(1:3))
    expect_equal(deque(1:3), as.deque(1:3))

    expect_output(print(d), "<Deque> of 3 elements:  num \\[1:3\\] 1 2 3")
    expect_equal(as.list(d), as.list(d$values()))
    expect_equal(as.vector(d), d$values())
})

