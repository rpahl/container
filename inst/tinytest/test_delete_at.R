ee = expect_equal

# --------------------
# delete_at.Container
# --------------------
co = container(a = 1, b = 2, f = mean, 3)
co2 = clone(co)
ee(delete_at(co), co2)
ee(delete_at(co, "a"), container(b = 2, f = mean, 3))
original_was_not_touched = ee(co, co2)
expect_true(original_was_not_touched)

ee(delete_at(co, "a"), delete_at(co, 1))
ee(delete_at(co, "b"), delete_at(co, 2))
ee(delete_at(co, 1:4), container())

ee(delete_at(co, "b", "a", 4:3, 1), container())
ee(delete_at(co, "a", 1), delete_at(co, 1))

expect_error(delete_at(co, "a", "x"), "names\\(s\\) not found: 'x'")
was_not_touched_on_error = ee(co, co2)
expect_true(was_not_touched_on_error)

expect_error(delete_at(co, "a", "x", "y"), "names\\(s\\) not found: 'x', 'y'")
expect_error(delete_at(co, "a", 1, 5), "index out of range \\(length = 4\\): 5")
was_not_touched_on_error = ee(co, co2)
expect_true(was_not_touched_on_error)

ee(delete_at_(co, 1:4), container())
ee(co, container())


# delete_at.Dict
d = dict(a = 1, b = 2, f = mean)
d2 = clone(d)
ee(delete_at(d, "a", "f", "b"), dict())
original_was_not_touched = ee(d, d2)
expect_true(original_was_not_touched)

# args as character vector
expect_true(is_empty(delete_at(d, names(d))))

ee(delete_at_(d, "a", "f", "b"), dict())
delete_was_done_on_original = ee(d, dict())
expect_true(delete_was_done_on_original)

d = dict(a = 1, b = 2, f = mean)
d2 = clone(d)
expect_error(delete_at_(d, "a", "x", "y"), "names\\(s\\) not found: 'x', 'y'")
was_not_touched_on_error = ee(d, d2)
expect_true(was_not_touched_on_error)


# -----------------
# delete.dict.table
# -----------------
d = dict.table(a = 1, b = 2, f = mean)
d2 = clone(d)
expect_true(is_empty(delete_at(d, "a", 2, "f")))
ee(d, d2)

ee(delete_at(d, "a", "a"), delete_at(d, "a"))
ee(delete_at(d, "a", 1), delete_at(d, "a"))
ee(delete_at(d, 1, "a", 1), delete_at(d, "a"))
ee(delete_at(d, 1:3), dict.table())
ee(delete_at(d, 1:3), delete_at(d, 3:1))

# args as character vector
expect_true(is_empty(delete_at(d, colnames(d))))

expect_error(delete_at_(d, "a", 4),
             "index out of range \\(ncol = 3\\): 4")
expect_error(delete_at_(d, "a", "z"),
             "column\\(s\\) not found: 'z'")

d_was_not_altered = ee(d, d2)
expect_true(d_was_not_altered)

ee(delete_at_(d, "b"), d2[, c(1, 3)])

expect_silent(delete_at_(d, "a"))

