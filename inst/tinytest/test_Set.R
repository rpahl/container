# Set constructor works as expected
s <- Set$new()
expect_true(s$empty())
expect_equal(s$length(), 0)
expect_false(s$has(NULL))
expect_false(NULL %e% s)
expect_equal(attr(s, "class"), c("Set", "Container", "Iterable", "R6"))

s <- Set$new(1, 1, 1)
expect_equal(s, Set$new(1))
expect_equal(mode(s$values()), "list")
expect_equal(Set$new(mean, mean, 1, 2), Set$new(mean, 1, 2))


# adding special elements works as expected
s <- Set$new(NULL)
expect_equal(s$length(), 1)
s$add(NULL) # is not added twice
expect_equal(s$length(), 1)
expect_equal(s$values(), list(NULL))

s$add(list())
expect_equal(s$values(), list(NULL, list()))
s$add(list())
expect_equal(s$values(), list(NULL, list()))

s$add(numeric(0))
expect_equal(s$values(), list(NULL, list(), numeric(0)))

