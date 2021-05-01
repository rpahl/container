ee = expect_equal

# -----------
# update.Dict
# -----------
# a Dict can be updated by another Dict object
d1 <- dict(A = 1, B = 2, C = 12)
d1.copy = clone(d1)
d2 <- dict(              C = 3, D = 4)
expect_error(update(d1, list()), "'other' must be a Dict")
ee(update(d1, dict()), d1)
ee(update(d1, d2), dict(A = 1, B = 2, C = 3, D = 4))

d1_was_not_touched = ee(d1, d1.copy)
expect_true(d1_was_not_touched)

# Update by reference
ee(update_(d1, d2), dict(A = 1, B = 2, C = 3, D = 4))
d1_was_changed_by_reference = ee(d1, dict(A = 1, B = 2, C = 3, D = 4))
expect_true(d1_was_changed_by_reference)

ee(update(dict(), d2), d2)


# -----------------
# update.dict.table
# -----------------
dit1 = dict.table(a = 1:2, b = 3:4)
dit1.copy = copy(dit1)
dit2 = dict.table(         b = 5:6, c = 8:9)
ee(update(dit1, dit2), dict.table(a = 1:2, b = 5:6, c = 8:9))

dit1_was_not_touched = ee(dit1, dit1.copy)
expect_true(dit1_was_not_touched)

# Update by reference
ee(update_(dit1, dit2), dict.table(a = 1:2, b = 5:6, c = 8:9))
dit1_was_changed = ee(dit1, dict.table(a = 1:2, b = 5:6, c = 8:9))
expect_true(dit1_was_changed)



# -----------
# update.list
# -----------
l1 = list(1, b = 2)
l2 = list(   b = 0, c = 3)
ee(update(l1, l2), list(1, b = 0, c = 3))
expect_error(update(l2, l1), "all elements of 'other' must be named")

ee(update(l1, list()), l1)
ee(update(l2, list()), l2)
ee(update(list(), l2), l2)

