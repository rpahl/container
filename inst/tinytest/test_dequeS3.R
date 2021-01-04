# Deque constructor
d <- deque()
expect_equal(attr(d, "class"), c("Deque", "Container", "Iterable", "R6"))
expect_true(is.deque(d))


# an element can be added to the left of a Deque
expect_equal(values(addleft(deque(0), 1)), as.list(1:0))
expect_equal(values(addleft(deque(mean), median)), list(median, mean))


# number of element occurrences can be counted
names <- deque("Lisa", "Bob", "Bob")
expect_equal(count(names, "Lisa"), 1)
expect_equal(count(names, "Bob"), 2)
expect_equal(count(names, "1"), 0)
expect_equal(count(add(names, "1"), "1"), 1)


# the left and rightmost element can be peeked
v <- 1:3
d <- as.deque(v)
expect_equal(peek(d), utils::tail(v, 1))
expect_equal(peekleft(d), utils::head(v, 1))

# the left and rightmost element can be popped
v <- 1:3
d <- as.deque(v)

expect_equal(pop(d), utils::tail(v, 1))
v <- v[-length(v)]
expect_equal(unlist(values(d)), v)
expect_equal(popleft(d), utils::head(v, 1))
v <- v[-1]
expect_equal(unlist(values(d)), v)


# popping empty Deques gives an error
expect_error(pop(deque()))
expect_error(popleft(deque()))


# a Deque can be reversed
v <- 1:10
expect_equal(unlist(values(rev(as.deque(v)))), rev(v))


# a Deque can be rotated
d <- deque()
expect_equal(d, rotate(d))
add(d, 1)
expect_equal(values(d), values(rotate(d)))
expect_equal(values(rotate(as.deque(1:2))), list(2, 1))
expect_equal(values(rotate(as.deque(1:3))), list(3, 1, 2))
expect_equal(values(rotate(as.deque(1:3), 2)), list(2, 3, 1))
expect_equal(values(rotate(as.deque(1:3), 3)), as.list(1:3))
expect_equal(values(rotate(as.deque(1:3), -1)), list(2, 3, 1))
expect_equal(values(rotate(as.deque(1:3), -2)), list(3, 1, 2))

v <- rnorm(10)
len <- length(v)
expect_equal(rotate(as.deque(v), 0), rotate(as.deque(v), len))
expect_equal(rotate(as.deque(v), len), rotate(as.deque(v), -len))
expect_equal(rotate(rotate(as.deque(v), 3), -3), as.deque(v))


# Conversion
co <- container(1, 2, 3)
d <- as.deque(co)
expect_equal(values(co), values(d))

v <- rnorm(10)
expect_equal(unlist(values(as.deque(v))), v)
expect_equal(as.list(d), values(d))

