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
d = dict(a = 1, b = 2)
expect_true(has(d, "a"))
expect_false(has(d, "x"))
expect_error(has(d, 1), "key must be character")

# ----------------
# has.dict.table
# ----------------
dit = dict.table(a = 1, b = 2)

expect_true(has(dit, "a"))
expect_true(has(dit, 2))
expect_false(has(dit, "x"))
expect_false(has(dit, 3))
expect_false(has(dit, 0))

