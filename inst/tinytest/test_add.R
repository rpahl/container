# -------------
# add.Container
# -------------
c1 = container(1)
expect_equal(add(c1, 2), container(1, 2))
c1_was_not_touched = all.equal(c1, container(1))
expect_true(c1_was_not_touched)

# --------
# add.Dict
# --------
d = dict(a = 1)
expect_equal(add(d, "b", 2), dict(a = 1, b = 2))
d_was_not_touched = all.equal(d, dict(a = 1))
expect_true(d_was_not_touched)

expect_error(add(d, "a", 2), "duplicated keys")

# --------------
# add.dict.table
# --------------
# TODO: unit tests


# -------------
# addleft.Deque
# -------------
d = deque(1, 2, 3)
expect_equal(add(d, 0), deque(1, 2, 3, 0))
expect_equal(addleft(d, 0), deque(0, 1, 2, 3))
d_was_not_touched = all.equal(d, as.deque(1:3))
expect_true(d_was_not_touched)

