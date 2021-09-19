ee = expect_equal

# ----------------
# update.Container
# ----------------
# a Container can be updated by another Dict object
co1 <- container(A = 1, B = 2, C = 12, 99, 100)
co1.copy = clone(co1)
co2 <- container(              C = 3, D = 4)
expect_error(update(co1, list()), "'other' must be a Container")
ee(update(co1, container()), co1)
ee(update(container(), co2), co2)
ee(update(co1, co2), container(A = 1, B = 2, C = 3, 99, 100, D = 4))
ee(update(co2, co1), container(C = 12, D = 4, A = 1, B = 2, 99, 100))

d1_was_not_touched = ee(co1, co1.copy)
expect_true(d1_was_not_touched)

# Update by reference
ee(ref_update(co1, co2), container(A = 1, B = 2, C = 3, 99, 100, D = 4))
d1_was_changed_by_reference = ee(co1, container(A = 1, B = 2, C = 3, 99, 100, D = 4))
expect_true(d1_was_changed_by_reference)


# -----------
# update.Dict
# -----------
# a Dict can be updated by another Dict object
d1 <- dict(A = 1, B = 2, C = 12)
d1.copy = clone(d1)
d2 <- dict(              C = 3, D = 4)
expect_error(update(d1, list()), "'other' must be a Dict")
ee(update(d1, dict()), d1)
ee(update(dict(), d2), d2)
ee(update(d1, d2), dict(A = 1, B = 2, C = 3, D = 4))

d1_was_not_touched = ee(d1, d1.copy)
expect_true(d1_was_not_touched)

# Update by reference
ee(ref_update(d1, d2), dict(A = 1, B = 2, C = 3, D = 4))
d1_was_changed_by_reference = ee(d1, dict(A = 1, B = 2, C = 3, D = 4))
expect_true(d1_was_changed_by_reference)



# -----------------
# update.dict.table
# -----------------
dit1 = dict.table(a = 1:2, b = 3:4)
dit1.copy = data.table::copy(dit1)
dit2 = dict.table(         b = 5:6, c = 8:9)
ee(update(dit1, dit2), dict.table(a = 1:2, b = 5:6, c = 8:9))

dit1_was_not_touched = ee(dit1, dit1.copy)
expect_true(dit1_was_not_touched)

# Update by reference
ee(ref_update(dit1, dit2), dict.table(a = 1:2, b = 5:6, c = 8:9))
dit1_was_changed = ee(dit1, dict.table(a = 1:2, b = 5:6, c = 8:9))
expect_true(dit1_was_changed)

dit3 = dict.table(a = 1:3, b = 3:5)
expect_error(update(dit1, dit3), "Supplied 3 items to be assigned to 2 items")

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

