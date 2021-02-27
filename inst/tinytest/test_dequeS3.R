# -----
# deque
# -----
d <- deque()
expect_true(is.deque(d))
expect_equal(length(d), 0)
expect_equal(names(d), NULL)
expect_equal(attr(d, "class"), c("Deque", "Container", "Iterable", "R6"))

# deque elements can be named
d <- deque(a = 2, 9, b = 1)
expect_equal(names(d), c("a", "", "b"))

# --------
# as.deque
# --------
expect_equal(as.deque(numeric()), deque())
expect_equal(as.deque(NULL), deque())
expect_equal(as.deque(list()), deque())
expect_equal(as.deque(1), deque(1))
expect_equal(as.deque(1:2), deque(1L, 2L))
expect_equal(as.deque(deque(1)), deque(1))

# deque can be created as copy from another deque
d = deque(1, 2)
d2 = as.deque(d)
expect_equal(d, d2)
d$clear()
# if d2 is a copy, it should not have been cleared
expect_equal(length(d), 0)
d2_was_also_cleaned = length(d2) == 0
expect_false(d2_was_also_cleaned )

# a data.frame can be converted to a deque
daf = data.frame(A = 1:2, B = 3:4)
expect_equal(as.list(as.deque(daf)), as.list(daf))

# a deque can be converted to a list
d = deque(1, b = 2)
expect_equal(as.list(as.deque(d)), list(1, b = 2))

# a set can be converted to a deque
s = setnew(1, 2)
expect_equal(as.list(as.deque(s)), list(1, 2))

# a dict can be converted to a deque
d = dict(a = 1, b = 2)
expect_equal(as.list(as.deque(d)), list(a = 1, b = 2))

# --------
# is.deque
# --------
expect_error(is.deque())
expect_false(is.deque(0))
expect_false(is.deque(list()))
expect_false(is.deque(container()))
expect_false(is.deque(dict()))
expect_false(is.deque(setnew()))

expect_true(is.deque(deque()))
expect_true(is.deque(deque(NULL)))
expect_true(is.deque(deque()))

# -------
# c.Deque
# -------
# Standard concatenate
d1 = deque(1)
expect_equal(c(d1, NULL), d1)
expect_equal(c(d1, list()), d1)
expect_equal(c(d1, numeric()), d1)
expect_equal(c(d1, 2:3), as.deque(1:3))
d2 = deque(2)
expect_equal(c(d1, d2, d2), deque(1, 2, 2))



# ----------
# S3 methods
# ----------

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
expect_equal(as.deque(NULL), deque())

v <- rnorm(10)
expect_equal(unlist(values(as.deque(v))), v)
expect_equal(as.list(d), values(d))

