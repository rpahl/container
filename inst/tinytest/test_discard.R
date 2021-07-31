ee = expect_equal

# -----------------
# discard.Container
# -----------------
co = container("a", "b", "a", mean, mean, NULL)
co2 = clone(co)
ee(discard(co), co2)
ee(discard(co, "a", mean, NULL), container("b", "a", mean))
original_was_not_touched = ee(co, co2)
expect_true(original_was_not_touched)

ee(discard_(co, "a", mean, NULL), container("b", "a", mean))
discard_was_done_on_original = ee(co, container("b", "a", mean))
expect_true(discard_was_done_on_original)
ee(discard(co, "x"), co)

co2 = clone(co)
nothing_was_removed = ee(co, co2)
expect_true(nothing_was_removed)

d = dict(a = 1, b = 2, f = mean)
d2 = clone(d)
ee(discard(d, 1, 2, mean), dict())
original_was_not_touched = ee(d, d2)
expect_true(original_was_not_touched)
ee(discard(d, "x"), d)

