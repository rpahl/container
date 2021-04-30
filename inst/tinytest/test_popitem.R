ee = expect_equal

# ------------------
# popitem.Container
# ------------------
co = container()
expect_error(popitem(container()))

co = container(1, 2, 3)
expect_true(popitem(co) %in% 1:3)
expect_equal(length(co), 2)

# ------------
# popitem.Set
# ------------
s = setnew()
expect_error(popitem(setnew()))

s = setnew(1, 2, 3)
expect_true(popitem(s) %in% 1:3)
expect_equal(length(s), 2)


# --------------
# popitem.Deque
# --------------
d = deque()
expect_error(popitem(deque()))

d = deque(1, 2, 3)
expect_true(popitem(d) %in% 1:3)
expect_equal(length(d), 2)

# -------------
# popitem.Dict
# -------------
expect_error(popitem(dict()))

d = dict(a = 1, b = 2, c = 3)
expect_true(popitem(d) %in% 1:3)
expect_equal(length(d), 2)

# -------------------
# popitem.dict.table
# -------------------
expect_error(popitem(dict.table()))

dit = dict.table(a = 1, b = 4)
expect_true(popitem(dit) %in% c(1, 4))
expect_equal(length(dit), 1)

