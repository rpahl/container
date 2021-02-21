# ------
# setnew
# ------
s <- setnew()
expect_true(is.set(s))
expect_equal(length(s), 0)
expect_equal(names(s), NULL)
expect_equal(attr(s, "class"), c("Set", "Container", "Iterable", "R6"))

# ------
# as.set
# ------
expect_equal(as.set(numeric()), setnew())
expect_equal(as.set(NULL), setnew())
expect_equal(as.set(list()), setnew())
expect_equal(as.set(1), setnew(1))
expect_equal(as.set(1:2), setnew(1L, 2L))
expect_equal(as.set(setnew(1)), setnew(1))

# set can be created as copy from another set
s = setnew(1, 2)
s2 = as.set(s)
expect_equal(s, s2)
s$clear()
expect_equal(length(s), 0)
expect_equal(length(s2), 2)

# a data.frame can be converted to a set
daf = data.frame(A = 1:2, B = 3:4)
expect_equal(as.list(as.set(daf)), as.list(daf))

# a set can be converted to a set
s = setnew(1, 2)
expect_equal(as.list(as.set(s)), list(1, 2))

# a deque can be converted to a set
d = deque(1, 2)
expect_equal(as.list(as.set(d)), list(1, 2))

# a dict can be converted to a set
d = dict(a = 1, b = 2)
expect_equal(as.list(as.set(d)), list(a = 1, b = 2))

# ------
# is.set
# ------
expect_error(is.set())
expect_false(is.set(0))
expect_false(is.set(list()))
expect_false(is.set(container()))
expect_false(is.set(dict()))
expect_false(is.set(deque()))

expect_true(is.set(setnew()))
expect_true(is.set(setnew(NULL)))
expect_true(is.set(setnew()))

