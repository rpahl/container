# -------------
# clear.Container
# -------------
# Clear works by reference
co = container(1, 2, mean)
clear(co)
expect_true(empty(co))


# --------------
# clear.dict.table
# --------------
d = dict.table(a = 1, b = 2)
clear(d)
expect_true(empty(d))

