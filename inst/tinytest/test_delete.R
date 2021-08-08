ee = expect_equal

# ----------------
# delete.Container
# ----------------
co = container("a", "b", "a", mean, mean, NULL)
co2 = clone(co)
ee(delete(co), co2)
ee(delete(co, "a", mean, NULL), container("b", "a", mean))
original_was_not_touched = ee(co, co2)
expect_true(original_was_not_touched)

ee(ref_delete(co, "a", mean, NULL), container("b", "a", mean))
delete_was_done_on_original = ee(co, container("b", "a", mean))
expect_true(delete_was_done_on_original)

co2 = clone(co)
expect_error(ref_delete(co, "a", "x"), '"x" is not in Container')
was_not_touched_on_error = ee(co, co2)
expect_true(was_not_touched_on_error)

expect_silent(ref_delete(co, "a"))

d = dict(a = 1, b = 2, f = mean)
d2 = clone(d)
ee(delete(d, 1, 2, mean), dict())
original_was_not_touched = ee(d, d2)
expect_true(original_was_not_touched)

