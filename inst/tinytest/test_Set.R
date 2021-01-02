# Set constructor
s <- Set$new()
expect_true(s$empty())
expect_equal(s$length(), 0)
expect_false(s$has(NULL))
expect_false(NULL %e% s)
expect_equal(attr(s, "class"), c("Set", "Container", "Iterable", "R6"))

s <- Set$new(1, 1, 1)
expect_equal(s, Set$new(list(1)))
expect_equal(mode(s$values()), "list")
expect_equal(Set$new(mean, mean, 1, 2), Set$new(mean, 1, 2))

s <- Set$new(rep(1:4, 2))
expect_equal(mode(s$values()), "numeric")
expect_equal(s$values(), 1:4)

