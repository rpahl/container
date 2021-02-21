# ------
# setnew
# ------
s <- setnew()
expect_true(is.set(s))
expect_equal(length(s), 0)
expect_equal(names(s), NULL)
expect_equal(attr(s, "class"), c("Set", "Container", "Iterable", "R6"))

# ------
# as.set
# ------

