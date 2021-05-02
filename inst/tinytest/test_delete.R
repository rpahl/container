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

# -----------
# delete.Dict
# -----------
d = dict(a = 1, b = 2, f = mean)
d2 = clone(d)
ee(delete(d, "a", "f", "b"), dict())
original_was_not_touched = ee(d, d2)
expect_true(original_was_not_touched)

# args as character vector
expect_true(is_empty(delete(d, names(d))))

ee(delete_(d, "a", "f", "b"), dict())
delete_was_done_on_original = ee(d, dict())
expect_true(delete_was_done_on_original)

d = dict(a = 1, b = 2, f = mean)
d2 = clone(d)
expect_error(delete_(d, "a", "x", "y"), "key\\(s\\) not found: x, y")
was_not_touched_on_error = ee(d, d2)
expect_true(was_not_touched_on_error)

expect_silent(delete_(d, "a"))

# -----------------
# delete.dict.table
# -----------------
d = dict.table(a = 1, b = 2, f = mean)
d2 = clone(d)
expect_true(is_empty(delete(d, "a", 2, "f")))
ee(d, d2)

# args as character vector
expect_true(is_empty(delete(d, colnames(d))))

expect_error(delete_(d, "a", 4),
             "indices out of range \\(ncol = 3\\): 4")
expect_error(delete_(d, "a", "z"),
             "column\\(s\\) not found: z")

d_was_not_altered = ee(d, d2)
expect_true(d_was_not_altered)

ee(delete_(d, "b"), d2[, c(1, 3)])

expect_silent(delete_(d, "a"))

