ee = expect_equal

# ------------------
# popitem_.Container
# ------------------
co = container()
expect_error(popitem_(container()))

co = container(1, 2, 3)
expect_true(popitem_(co) %in% 1:3)
expect_equal(length(co), 2)

# ------------
# popitem_.Set
# ------------
s = setnew()
expect_error(popitem_(setnew()))

s = setnew(1, 2, 3)
expect_true(popitem_(s) %in% 1:3)
expect_equal(length(s), 2)


# --------------
# popitem_.Deque
# --------------
d = deque()
expect_error(popitem_(deque()))

d = deque(1, 2, 3)
expect_true(popitem_(d) %in% 1:3)
expect_equal(length(d), 2)

# -------------
# popitem_.Dict
# -------------
expect_error(popitem_(dict()))

d = dict(a = 1, b = 2, c = 3)
expect_true(popitem_(d) %in% 1:3)
expect_equal(length(d), 2)

# -------------------
# popitem_.dict.table
# -------------------
expect_error(popitem_(dict.table()))

dit = dict.table(a = 1, b = 4)
expect_true(popitem_(dit) %in% c(1, 4))
expect_equal(length(dit), 1)

