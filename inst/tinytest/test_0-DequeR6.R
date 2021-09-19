# ----------
# initialize
# ----------
d <- Deque$new()
expect_equal(attr(d, "class"), c("Deque", "Container", "Iterable", "R6"))


# -------
# addleft
# -------
# an element can be added to the left of a Deque
expect_equal(Deque$new(0L)$addleft(1)$values(), as.list(1:0))
expect_equal(Deque$new(mean)$addleft(median)$values(), list(median, mean))

# several named elements can be added to the left
d = Deque$new(0)
d$addleft(1, "a")$addleft(2)
expect_equal(d, Deque$new(2, a = 1, 0))


# -----------------
# peek and peekleft
# -----------------
# the left and rightmost element can be peeked
v <- 1:3
d <- Deque$new(1, 2, 3)
expect_equal(d$peek(), utils::tail(v, 1))
expect_equal(d$peekleft(), utils::head(v, 1))


# ---------------
# pop and popleft
# ---------------
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


# ---
# rev
# ---
# a Deque can be reversed
v <- list(1, 2, 3)
expect_equal(Deque$new(1, 2, 3)$rev()$values(), rev(v))

# rev works also on empty elements
d <- Deque$new(list(), NULL, NA, numeric())
l <- as.list(d)
expect_equal(d$length(), 4)
expect_equal(as.list(d$rev()), rev(l))

# ------
# rotate
# ------
d <- Deque$new()
expect_equal(d$rotate(), Deque$new())

d <- Deque$new(1)
expect_equal(d$rotate(1), Deque$new(1))
expect_equal(d$rotate(-1), Deque$new(1))

d <- Deque$new(1, 2)
d12 <- Deque$new(1, 2)
d21 <- Deque$new(2, 1)
expect_equal(d$rotate(), d21)
expect_equal(d$rotate(2), d21)
expect_equal(d$rotate(-2), d21)
expect_equal(d$rotate(-1), d12)
expect_equal(d$rotate(4), d12)

# rotate works also with empty elements
d <- Deque$new(list(), NULL, NA, numeric())
l <- as.list(d)

expect_equal(as.list(d$rotate()), l[c(4, 1, 2, 3)])
expect_equal(as.list(d$rotate(-1)), l)
expect_equal(as.list(d$rotate(3)), l[c(2, 3, 4, 1)])




