ee = expect_equal

# ----------------
# delete.Container
# ----------------
co = container("a", "b", "a", mean, mean, NULL)
co2 = clone(co)
ee(delete(co), co2)
ee(delete(co, "a", mean, NULL), container("a", "b", mean))
original_was_not_touched = ee(co, co2)
expect_true(original_was_not_touched)

ee(delete_(co, "a", mean, NULL), container("a", "b", mean))
delete_was_done_on_original = ee(co, container("a", "b", mean))
expect_true(delete_was_done_on_original)

co2 = clone(co)
expect_error(delete_(co, "a", "x"), '"x" is not in Container')
was_not_touched_on_error = ee(co, co2)
expect_true(was_not_touched_on_error)

expect_silent(delete_(co, "a"))

d = dict(a = 1, b = 2, f = mean)
d2 = clone(d)
ee(delete(d, 1, 2, mean), dict())
original_was_not_touched = ee(d, d2)
expect_true(original_was_not_touched)

