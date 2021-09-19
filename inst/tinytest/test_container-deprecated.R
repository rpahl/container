# verify that functions are deprecated
co <- Container$new(1L)
expect_warning(co$empty(), "deprecated")
expect_warning(co$remove(), "deprecated")
expect_warning(co$size(), "deprecated")
expect_warning(co$type(), "deprecated")

# Dict set is deprecated and replaced by replace
d <- Dict$new(a = 1, b = 2)
expect_warning(d$set("b", 9), "Use 'replace' instead.")
expect_equal(d$at2("b"), 9)

# Dict remove is deprecated and replaced by delete
d <- Dict$new(a = 1, b = 2)
expect_warning(d$remove("b"), "Use 'delete' instead.")
expect_false(d$has("b"))

# Dict sort is deprecated
d <- Dict$new(b = 1, a = 2)
expect_warning(d$sort(), "'sort' is deprecated - keys are now always sorted")

expect_warning(keys(dict()), "'keys' is deprecated.")

d <- Dict$new(b = 1, a = 2)
expect_warning(sortkey(d),
               "'sort' is deprecated - keys are now always sorted")
