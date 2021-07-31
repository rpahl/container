ee = expect_equal

# -----------------
# discard.Container
# -----------------
co = container(a = 1, b = 2, f = mean, 3)
co2 = clone(co)
ee(discard_at(co), co2)
ee(discard_at(co, "a"), container(b = 2, f = mean, 3))
original_was_not_touched = ee(co, co2)
expect_true(original_was_not_touched)

ee(discard_at(co, "a"), discard_at(co, 1))
ee(discard_at(co, "b"), discard_at(co, 2))
ee(discard_at(co, 1:4), container())

ee(discard_at(co, "b", "a", 4:3, 1), container())
ee(discard_at(co, "a", 1), discard_at(co, 1))

ee(discard_at(co, "a", "x"), discard_at(co, "a"))
ee(discard_at(co, "x", "a"), discard_at(co, "a"))
ee(discard_at(co, "a", 5), discard_at(co, "a"))
ee(discard_at(co, 6, "a", 5), discard_at(co, "a"))

ee(discard_at_(co, 1:4), container())
ee(co, container())

# discard_at.Dict
d = dict(a = 1, b = 2, f = mean)
d2 = clone(d)
ee(discard_at(d, "a", "f", "b"), dict())
original_was_not_touched = ee(d, d2)
expect_true(original_was_not_touched)

# args as character vector
expect_true(is_empty(discard_at(d, names(d))))

ee(delete_at_(d, "a", "f", "b"), dict())
discard_was_done_on_original = ee(d, dict())
expect_true(discard_was_done_on_original)



# ------------------
# discard.dict.table
# ------------------
d = dict.table(a = 1, b = 2, f = mean)
d2 = clone(d)
expect_true(is_empty(discard_at(d, 1, "b", 3)))
expect_true(is_empty(discard_at(d, 1:3)))
expect_true(is_empty(discard_at(d, 3:1)))
ee(d, d2)

# args as character vector
expect_true(is_empty(discard_at(d, colnames(d))))
expect_true(is_empty(discard_at(d, rev(colnames(d)))))
ee(d, d2)

expect_silent(discard_at_(d, "x", 4, 11))
d_was_not_altered = ee(d, d2)
expect_true(d_was_not_altered)

ee(discard_at_(d, "b"), d2[, c(1, 3)])
expect_silent(discard_at_(d, "a"))
expect_false(ee(d, d2))

