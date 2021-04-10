ee = expect_equal

# ---------------
# clear.Container
# ---------------
co = container(1, 2, mean)
ee(clear(co), container())
expect_false(is_empty(co))
ee(clear_(co), container())
expect_true(is_empty(co))

# ----------------
# clear.dict.table
# ----------------
dit = dict.table(a = 1, b = 2)
ee(clear(dit), dict.table())
expect_false(is_empty(dit))
ee(clear_(dit), dict.table())
expect_true(is_empty(dit))

