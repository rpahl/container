ee = expect_equal

# -----------------
# replace.Container
# -----------------
x = container(1, "z")
ee(replace(x, 1, 0), container(0, "z"))
ee(replace(x, "z", 0), container(1, 0))

x_was_not_touched = all.equal(x, container(1, "z"))
expect_true(x_was_not_touched)

# -----------
# replace.Set
# -----------
x = setnew(1, "z")
ee(replace(x, 1, 0), setnew(0, "z"))
ee(replace(x, "z", 0), setnew(0, 1))

x_was_not_touched = all.equal(x, setnew(1, "z"))
expect_true(x_was_not_touched)

# -------------
# replace.Deque
# -------------
x = deque(1, "z")
ee(replace(x, 1, 0), deque(0, "z"))
ee(replace(x, "z", 0), deque(1, 0))

x_was_not_touched = all.equal(x, deque(1, "z"))
expect_true(x_was_not_touched)

# ------------------
# replace.dict.table
# ------------------
x = deque(1, "z")
ee(replace(x, 1, 0), deque(0, "z"))
ee(replace(x, "z", 0), deque(1, 0))

x_was_not_touched = all.equal(x, deque(1, "z"))
expect_true(x_was_not_touched)


