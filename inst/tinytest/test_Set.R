# Set constructor works as expected
s <- Set$new()
expect_true(s$empty())
expect_equal(s$length(), 0)
expect_false(s$has(NULL))
#expect_false(NULL %e% s) # TODO: %e% operator
expect_equal(attr(s, "class"), c("Set", "Container", "Iterable", "R6"))

expect_equal(Set$new(1, 1, 1), Set$new(1))
expect_equal(Set$new(NULL, NULL), Set$new(NULL))
expect_equal(Set$new(mean, mean, 1, 2), Set$new(mean, 1, 2))


# adding zero-length elements works as expected
s <- Set$new(NULL)
expect_equal(s$length(), 1)
s$add(NULL) # is not added twice
expect_equal(s$length(), 1)
expect_equal(as.list(s$values()), list(NULL))

s$add(list())
expect_equal(as.list(s$values()), list(NULL, list()))
s$add(list())
expect_equal(as.list(s$values()), list(NULL, list()))

s$add(numeric(0))
expect_equal(as.list(s$values()), list(NULL, list(), numeric(0)))

# Output of zero-length elements looks as expected
out = utils::capture.output(s)
expect_equal(out[[1]], "<<Set(3)>> ")
expect_equal(out[[2]], "[<<NULL>>, list(0), numeric(0)]")

