# ---------
# Container
# ---------
co <- container(1, 2)
expect_equal(values(co + 1), list(1, 2, 1))
expect_equal(co + 1, 1 + co)
expect_equal(values(co + co), rep(values(co), 2))


# -----
# Deque
# -----
d <- deque(1, 2)
expect_equal(values(d + 1), list(1, 2, 1))
expect_equal(values(1 + d), list(1, 1, 2))
expect_equal(values(d + d), rep(values(d), 2))


# ---
# Set
# ---
l1 <- list(1, 2, 3,    "A", "B", "C")
l2 <- list(   2, 3, 4,      "B", "C", "D")
s1 <- setnew(l1)
s2 <- setnew(l2)
expect_equal(values(s1 + s2), union(l1, l2))
expect_true(setequal(values(s1 + s2), values(s2 + s1)))
expect_equal(values(s1 / s2), intersect(l1, l2))
expect_true(setequal(values(s1 / s2), values(s2 / s1)))
expect_equal(values(s1 - s2), setdiff(l1, l2))
expect_false(setequal(values(s1 - s2), values(s2 - s1)))
expect_equal(values(s2 - s1), setdiff(l2, l1))


# ----
# Dict
# ----
# x + y updates x by y
lx = list(a = 1, b = 2)
ly = list(       b = 9, c = 1)
x <- dict(lx)
y <- dict(ly)
xy <- x + y
expect_equal(values(x), lx)  # verify x was not touched
expect_equal(values(y), ly)  # verify y was not touched
expect_equal(length(xy), 3)
expect_equal(values(xy), values(update(x, y)))
expect_equal(x + x, x)
expect_equal(x + dict(), x)
expect_equal(dict() + x, x)
expect_error(x + 1, "both arguments must be dicts")
expect_error(1 + x, "both arguments must be dicts")
expect_equal(length(y), 2)  # verify y was not touched

# x - y returns a copy of x with all `y` keys being discarded
lx = list(a = 1, b = 2)
ly = list(       b = 9, c = 1)
x <- dict(lx)
y <- dict(ly)
expect_equal(x - y, dict(a = 1))
expect_equal(y - x, dict(c = 1))
expect_equivalent(x - x, dict())
expect_equivalent(dict() - x, dict())
expect_equal(values(x), lx)  # verify x was not touched
expect_equal(values(y), ly)  # verify y was not touched

# x / y returns a copy keeping only keys exiting in both
lx = list(a = 1, b = 2)
ly = list(       b = 9, c = 1)
x <- dict(lx)
y <- dict(ly)
expect_equal(values(x / y), list(b = 2))
expect_equal(values(y / x), list(b = 9))
expect_equal(x / x, x)
expect_equal(x / dict(), dict())
expect_equal(dict() / x, dict())
expect_equal(values(x), lx)  # verify x was not touched
expect_equal(values(y), ly)  # verify y was not touched

