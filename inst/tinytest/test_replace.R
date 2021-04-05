ee = expect_equal

# -----------------
# replace.Container
# -----------------
x = container(1, "z")
ee(replace(x, 1, 0), container(0, "z"))
ee(replace(x, "z", 0), container(1, 0))
ee(replace(x, "z", NULL), container(1, NULL))

expect_error(replace(x, old = 99, 0),
             "old element \\(99\\) is not in Container")
ee(replace(x, old = 99, 0, add = TRUE), container(1, "z", 0))

x_was_not_touched = all.equal(x, container(1, "z"))
expect_true(x_was_not_touched)

ee(replace(x, 1, 0, copy = FALSE), container(0, "z"))
was_changed_by_referene = ee(x,  container(0, "z"))
expect_true(was_changed_by_referene)


# -----------
# replace.Set
# -----------
x = setnew(1, "z")
ee(replace(x, 1, 0), setnew(0, "z"))
ee(replace(x, "z", 0), setnew(0, 1))

expect_error(replace(x, old = 99, 0), "99 is not in Set")

x_was_not_touched = all.equal(x, setnew(1, "z"))
expect_true(x_was_not_touched)

ee(replace(x, 1, 0, copy = FALSE), setnew(0, "z"))
was_changed_by_referene = ee(x,  setnew(0, "z"))
expect_true(was_changed_by_referene)


# -------------
# replace.Deque
# -------------
x = deque(1, "z")
ee(replace(x, 1, 0), deque(0, "z"))
ee(replace(x, "z", 0), deque(1, 0))
ee(replace(x, "z", NULL), deque(1, NULL))

expect_error(replace(x, old = 99, 0),
             "old element \\(99\\) is not in Deque")
ee(replace(x, old = 99, 0, add = TRUE), deque(1, "z", 0))

x_was_not_touched = all.equal(x, deque(1, "z"))
expect_true(x_was_not_touched)

ee(replace(x, 1, 0, copy = FALSE), deque(0, "z"))
was_changed_by_referene = ee(x,  deque(0, "z"))
expect_true(was_changed_by_referene)

# ------------
# replace.Dict
# ------------
d = dict(a = 1)
ee(replace(d, "a", 1:5), dict(a = 1:5))
expect_error(replace(d, "b", 2), "key 'b' not in Dict")
ee(replace(d, "b", 2, add = TRUE), dict(a = 1, b = 2))

d_was_not_touched = all.equal(d, dict(a = 1))
expect_true(d_was_not_touched)

ee(replace(d, "a", 1:5, copy = FALSE), dict(a = 1:5))
was_changed_by_referene = ee(d, dict(a = 1:5))
expect_true(was_changed_by_referene)

# ------------------
# replace.dict.table
# ------------------
dit = dict.table(a = 1:3)
ee(replace(dit, "a", 3:1), dict.table(a = 3:1))
expect_error(replace(dit, "b", 4:6), "column 'b' not in dict.table")
ee(replace(dit, "b", 4:6, add = TRUE), dict.table(a = 1:3, b = 4:6))

dit_was_not_touched = all.equal(dit, dict.table(a = 1:3))
expect_true(dit_was_not_touched)

ee(replace(dit, "a", 6:4, copy = FALSE), dict.table(a = 6:4))
was_changed_by_referene = ee(dit, dict.table(a = 6:4))
expect_true(was_changed_by_referene)

