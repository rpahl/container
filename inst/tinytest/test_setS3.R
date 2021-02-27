# ------
# setnew
# ------
s <- setnew()
expect_true(is.set(s))
expect_equal(length(s), 0)
expect_equal(names(s), NULL)
expect_equal(attr(s, "class"), c("Set", "Container", "Iterable", "R6"))

# set elements can be named
s <- setnew(a = 2, b = 1, 9)
expect_equal(names(s), c("b", "a", ""))

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

# a set can be converted to a list
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

# -----
# c.Set
# -----
# Standard concatenate
s1 = setnew(1L)
expect_equal(c(s1, NULL), s1)
expect_equal(c(s1, list()), s1)
expect_equal(c(s1, numeric()), s1)
expect_equal(c(s1, 2:3), as.set(1:3))
s2 = setnew(2L)
expect_equal(c(s1, s2, s2), setnew(1L, 2L, 2L))

# Ensure concatenated objects are always copies also for nested containers
ss1 = setnew(s1)
sss1 = setnew(ss1)

ss = c(s1, ss1, sss1)
expect_equal(unpack(ss), c(1, 1, 1))
s1$add(3)
ss1$add(3)
sss1$add(3)

# If concatenation was done with copies, the following must still hold
expect_equal(unpack(ss), c(1, 1, 1))

