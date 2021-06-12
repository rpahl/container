ee = expect_equal

# ---------------
# has.Container
# ---------------
co = container(1, 2, mean)
expect_true(has(co, 1))
expect_true(has(co, mean))
expect_false(has(co, 1:2))
expect_false(has(co, NA))
expect_false(has(co, NULL))
expect_error(has(co, 1, 2), "unused argument")

# --------
# has.Dict
# --------
d = dict(a = 1, b = 3)
expect_false(has(d, "a"))
expect_true(has(d, 1))
expect_false(has(d, 2))

# ----------------
# has.dict.table
# ----------------
dit = dict.table(a = 1:3, b = as.list(4:6))

expect_true(has(dit, 1:3))
expect_false(has(dit, 4:6))
expect_true(has(dit, as.list(4:6)))

expect_warning(has(dit, 1:2),
    "length of column vector \\(2\\) does not match number of rows \\(3\\)")


