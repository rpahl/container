# ---------
# Container
# ---------
# x + y
co <- container(1, 2)
expect_equal(co + container(), co)
expect_equal(container() + co, co)
expect_equal(co + list(), co)
expect_equal(list() + co, co)
expect_equal(co + 1, container(1, 2, 1))
expect_equal(1 + co, container(1, 1, 2))
expect_equal(1:3 + co, container(1, 2, 3, 1, 2))
expect_equal(co + co, container(1, 2, 1, 2))

# x - y
co <- container(1L, 2L, 2L, 3L)
expect_equal(co - container(), co)
expect_equal(container() - co, container())
expect_equal(co - co, container())
expect_equal(co - list(), co)
expect_equal(list() - co, container())
expect_equal(co - 1L, container(2, 2, 3))
expect_equal(1L - co, container())
expect_equal(co - 1:4, container(2))
expect_equal(1:4 - co, container(4))


# -----
# Deque
# -----
d <- deque(1, 2)
expect_equal(d + deque(), d)
expect_equal(deque() + d, d)
expect_equal(list() + d, d)
expect_equal(d + list(), d)
expect_equal(d + 1, deque(1, 2, 1))
expect_equal(1 + d, deque(1, 1, 2))
expect_equal(d + d, deque(1, 2, 1, 2))

d <- deque(1L, 2L, 2L, 3L)
expect_equal(d - deque(), d)
expect_equal(d - d, deque())
expect_equal(deque() - d, deque())
expect_equal(list() - d, deque())
expect_equal(d - list(), d)
expect_equal(d - 1L, deque(2, 2, 3))
expect_equal(1L - d, deque())
expect_equal(d - 1:4, deque(2))
expect_equal(1:4 - d, deque(4))


# ---
# Set
# ---
# x + y
x <- setnew(1, 2,    "1", "2")
y <- setnew(   2, 3,      "2", "3")
expect_equal(x + x, x)
expect_equal(x + setnew(), x)
expect_equal(setnew() + x, x)
expect_equal(x + list(), x)
expect_equal(list() + x, x)
expect_true((x + y) == (y + x))

# x - y
x <- setnew(1, 2,    "1", "2")
y <- setnew(   2, 3,      "2", "3")
expect_equal(x - x, setnew())
expect_equal(x - setnew(), x)
expect_equal(setnew() - x, setnew())
expect_equal(x - list(), x)
expect_equal(list() - x, setnew())
expect_true(x - y != y - x)


# x & y
x <- setnew(1, 2,    "1", "2")
y <- setnew(   2, 3,      "2", "3")
expect_equal(intersect(as.list(x), as.list(y)), list(2)) # 'wrong' base implementation
expect_equal(as.list(x & y), list(2, "2"))  # correct
expect_equal(x & x, x)
expect_equal(x & setnew(), setnew())
expect_equal(setnew() & x, setnew())
expect_equal(list() & x, setnew())
expect_equal(x & list(), setnew())
expect_true((x & y) == (y & x))


# ----
# Dict
# ----
# x + y updates x by y
x <- dict(a = 1, b = 2)
y <- dict(       b = 9, c = 1)
expect_equal(x + x, x)
expect_equal(x + dict(), x)
expect_equal(dict() + x, x)
expect_equal(x + list(), x)
expect_equal(list() + x, x)
expect_equal(x + y, dict(a = 1, b = 9, c = 1))
expect_equal(y + x, dict(a = 1, b = 2, c = 1))
expect_equal(x + as.list(y), dict(a = 1, b = 9, c = 1))
expect_true(isTRUE(all.equal(x + y, x + y)))
expect_false(isTRUE(all.equal(x + y, y + x)))

# x - y returns a copy of x with all `y` keys being discarded
x <- dict(a = 1, b = 2)
y <- dict(       b = 9, c = 1)
expect_equal(x - x, dict())
expect_equal(x - dict(), x)
expect_equal(x - as.list(x), dict())
expect_equal(dict() - x, dict())
expect_equal(x - list(), x)
expect_equal(list() - x, dict())
expect_equal(x - y, dict(a = 1))
expect_equal(x - as.list(y), dict(a = 1))
expect_equal(y - x, dict(c = 1))
expect_equal(y - as.list(x), dict(c = 1))

# x & y returns a copy of the first dict with all keys being removed that are
# not common in both x and y
x <- dict(a = 1, b = 2)
y <- dict(       b = 9, c = 1)
expect_equal(x & x, x)
expect_equal(x & dict(), dict())
expect_equal(dict() & x, dict())
expect_equal(x & list(), dict())
expect_equal(list() & x, dict())
expect_equal(x & y, dict(b = 2))
expect_equal(y & x, dict(b = 9))


