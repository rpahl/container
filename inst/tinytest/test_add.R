ee = expect_equal

# -------------
# add.Container
# -------------
x = container(1)
ee(add(x, 2, x = 3), container(1, 2, x = 3))
x_was_not_touched = all.equal(x, container(1))
expect_true(x_was_not_touched)

ref_add(x, 2)
ee(x, container(1, 2))

# set
# ---
x = setnew(1)
ee(add(x, 2, x = 3), setnew(1, 2, x = 3))
x_was_not_touched = all.equal(x, setnew(1))
expect_true(x_was_not_touched)

ref_add(x, 2)
ee(x, setnew(1, 2))

# deque
# -----
x = deque(1)
ee(add(x, 2, b = 3), deque(1, 2, b = 3))
x_was_not_touched = all.equal(x, deque(1))
expect_true(x_was_not_touched)

ref_add(x, 2)
ee(x, deque(1, 2))

# --------
# add.Dict
# --------
d = dict(a = 1)
ee(add(d), d)
ee(add(d, b = 2, x = 4), dict(a = 1, b = 2, x = 4))
d_was_not_touched = all.equal(d, dict(a = 1))
expect_true(d_was_not_touched)

ref_add(d, b = 2, d = 4)
ee(d, dict(a = 1, b = 2, d = 4))

expect_error(ref_add(d, z = 9, b = 2), "name 'b' exists already")

d_was_not_touched_upon_error = all.equal(d, dict(a = 1, b = 2, d = 4))
expect_true(d_was_not_touched_upon_error)


expect_error(add(d, a = 2), "name 'a' exists already")
expect_error(add(d, 2), "all elements must be named")
expect_error(add(d, "a", 2), "all elements must be named")

# --------------
# add.dict.table
# --------------
dit = dict.table(a = 1)
ee(add(dit), dit)
ee(add(dit, b = 2, x = 3), dict.table(a = 1, b = 2, x = 3))
dit_was_not_touched = all.equal(dit, dict.table(a = 1))
expect_true(dit_was_not_touched)

ref_add(dit, b = 2, c = 3)
ee(dit, dict.table(a = 1, b = 2, c = 3))


expect_error(add(dit, d = 4, 5), "all elements must be named")
d_was_not_touched_upon_error = all.equal(dit, dict.table(a = 1, b = 2, c = 3))
expect_true(d_was_not_touched_upon_error)

expect_error(add(dit, d = 4, a = 5, b = 6), "names 'a', 'b' exist already")


# -------------
# addleft.Deque
# -------------
d = deque(1, 2, 3)
ee(add(d, n = 0, n1 = 1), deque(1, 2, 3, n = 0, n1 = 1))
ee(addleft(d, n0 = 0, n1 = 1),
             deque(n1 = 1, n0 = 0, 1, 2, 3))
d_was_not_touched = all.equal(d, as.deque(1:3))
expect_true(d_was_not_touched)

ref_addleft(d, 4)
ee(d, deque(4, 1, 2, 3))

