# Deque constructor
d <- Deque$new()
expect_equal(attr(d, "class"), c("Deque", "Container", "Iterable", "R6"))

# an element can be added to the left of a Deque
expect_equal(Deque$new(0L)$addleft(1)$values(), as.list(1:0))
expect_equal(Deque$new(mean)$addleft(median)$values(), list(median, mean))


# number of element occurrences can be counted
names <- Deque$new("Lisa", "Bob", "Bob")
expect_equal(names$count("Lisa"), 1)
expect_equal(names$count("Bob"), 2)
expect_equal(names$count("1"), 0)
expect_equal(names$add("1")$count("1"), 1)


# the left and rightmost element can be peeked
v <- 1:3
d <- Deque$new(1, 2, 3)
expect_equal(d$peek(), utils::tail(v, 1))
expect_equal(d$peekleft(), utils::head(v, 1))


# the left and rightmost element can be popped
v <- 1:3
d <- Deque$new(1, 2, 3)

expect_equal(d$pop(), utils::tail(v, 1))
v <- v[-length(v)]
expect_equal(d$values(), as.list(v))
expect_equal(d$popleft(), utils::head(v, 1))
v <- v[-1]
expect_equal(d$values(), as.list(v))


# popping empty Deques gives an error
expect_error(Deque$new()$pop(), "pop at empty Deque")
expect_error(Deque$new()$popleft(), "popleft at empty Deque")


# a Deque can be reversed
v <- list(1, 2, 3)
expect_equal(Deque$new(1, 2, 3)$rev()$values(), rev(v))


# a Deque can be rotated
d <- Deque$new()
expect_equal(d, d$rotate())
d$add(1)
expect_equal(d$values(), d$rotate()$values())
expect_equal(Deque$new(1, 2)$rotate()$values(), list(2, 1))
expect_equal(Deque$new(1, 2, 3)$rotate()$values(), list(3, 1, 2))
expect_equal(Deque$new(1, 2, 3)$rotate(2)$values(), list(2, 3, 1))
expect_equal(Deque$new(1, 2, 3)$rotate(3)$values(), list(1, 2, 3))
expect_equal(Deque$new(1, 2, 3)$rotate(-1)$values(), list(2, 3, 1))
expect_equal(Deque$new(1, 2, 3)$rotate(-2)$values(), list(3, 1, 2))

