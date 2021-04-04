ee = expect_equal

# -------------
# add.Container
# -------------
x = container(1)
ee(add(x, 2, b = 3), container(1, 2, b = 3))
x_was_not_touched = all.equal(x, container(1))
expect_true(x_was_not_touched)

# set
# ---
x = setnew(1)
ee(add(x, 2, b = 3), setnew(1, 2, b = 3))
x_was_not_touched = all.equal(x, setnew(1))
expect_true(x_was_not_touched)

# deque
# -----
x = deque(1)
ee(add(x, 2, b = 3), deque(1, 2, b = 3))
x_was_not_touched = all.equal(x, deque(1))
expect_true(x_was_not_touched)



# --------
# add.Dict
# --------
d = dict(a = 1)
ee(add(d, b = 2, d = 4), dict(a = 1, b = 2, d = 4))
d_was_not_touched = all.equal(d, dict(a = 1))
expect_true(d_was_not_touched)

expect_error(add(d, a = 2), "key 'a' already in Dict")
expect_error(add(d, 2), "all elements must be named")
expect_error(add(d, "a", 2), "all elements must be named")


# -------------
# addleft.Deque
# -------------
d = deque(1, 2, 3)
ee(add(d, n = 0, n1 = 1), deque(1, 2, 3, n = 0, n1 = 1))
ee(addleft(d, n0 = 0, n1 = 1),
             deque(n1 = 1, n0 = 0, 1, 2, 3))
d_was_not_touched = all.equal(d, as.deque(1:3))
expect_true(d_was_not_touched)

