ee = expect_equal

# ----------------
# delete.Container
# ----------------
co = container("a", "b", "a", mean, mean, NULL)
co2 = clone(co)
ee(discard(co), co2)
ee(discard(co, "a", mean, NULL), container("a", "b", mean))
original_was_not_touched = ee(co, co2)
expect_true(original_was_not_touched)

ee(discard_(co, "a", mean, NULL), container("a", "b", mean))
delete_was_done_on_original = ee(co, container("a", "b", mean))
expect_true(delete_was_done_on_original)

co2 = clone(co)
expect_silent(discard_(co, "x"))
nothing_was_removed = ee(co, co2)
expect_true(nothing_was_removed)

# -----------
# delete.Dict
# -----------
d = dict(a = 1, b = 2, f = mean)
d2 = clone(d)
ee(discard(d, "a", "f", "b"), dict())
original_was_not_touched = ee(d, d2)
expect_true(original_was_not_touched)

ee(discard_(d, "a", "f", "b"), dict())
delete_was_done_on_original = ee(d, dict())
expect_true(delete_was_done_on_original)

d = dict(a = 1, b = 2, f = mean)
d2 = clone(d)

expect_silent(discard_(d, "x", "y"))
nothing_was_removed = ee(d, d2)
expect_true(nothing_was_removed)


# -----------------
# delete.dict.table
# -----------------
d = dict.table(a = 1, b = 2, f = mean)
d2 = clone(d)
expect_true(is_empty(discard(d, "a", 2, "f")))
ee(d, d2)

expect_silent(discard_(d, "x", 4, 11))
d_was_not_altered = ee(d, d2)
expect_true(d_was_not_altered)

ee(discard_(d, "b"), d2[, c(1, 3)])
expect_silent(discard_(d, "a"))

