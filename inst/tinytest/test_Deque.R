# a Deque object can be created
d <- Deque$new()
expect_equal(attr(d, "class"), c("Deque", "Container", "Iterable", "R6"))

# an element can be added to the left of a Deque
expect_equal(Deque$new(0L)$addleft(3:1)$values(), as.integer(3:0))
expect_equal(Deque$new(mean)$addleft(median)$values(), list(median, mean))


# number of element occurrences can be counted
names <- Deque$new(c("Lisa", "Bob", "Bob"))
expect_equal(names$count("Lisa"), 1)
expect_equal(names$count("Bob"), 2)
expect_equal(names$count("1"), 0)
expect_equal(names$add("1")$count("1"), 1)


# the left and rightmost element can be peeked
v <- 1:3
d <- Deque$new(v)

expect_equal(d$peek(), utils::tail(v, 1))
expect_equal(d$peekleft(), utils::head(v, 1))


# printed Deque object looks as expected
expect_output(print(Deque$new(1:3)),
              "<Deque> of 3 elements:  int [1:3] 1 2 3", fixed = TRUE)

# the left and rightmost element can be popped
v <- 1:3
d <- Deque$new(v)

expect_equal(d$pop(), utils::tail(v, 1))
v <- v[-length(v)]
expect_equal(d$values(), v)
expect_equal(d$popleft(), utils::head(v, 1))
v <- v[-1]
expect_equal(d$values(), v)


# popping empty Deques gives an error
expect_error(Deque$new()$pop())
expect_error(Deque$new()$popleft())


# a Deque can be reversed
v <- 1:10
expect_equal(Deque$new(v)$rev()$values(), rev(v))


# a Deque can be rotated
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

