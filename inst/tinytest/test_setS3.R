# Set constructor works as expected
s <- setnew()
expect_true(is.set(s))
expect_true(empty(s))
expect_equal(length(s), 0)
expect_false(has(s, NULL))
expect_equal(attr(s, "class"), c("Set", "Container", "Iterable", "R6"))

s <- setnew(1, 1, 1)
expect_equal(s, setnew(1))
expect_equal(mode(values(s)), "list")
expect_equal(setnew(mean, mean, 1, 2), setnew(mean, 1, 2))


# adding special elements works as expected
s <- setnew(NULL)
expect_equal(length(s), 1)
add(s, NULL) # is not added twice
expect_equal(length(s), 1)
expect_equal(values(s), list(NULL))

add(s, list())
expect_equal(values(s), list(NULL, list()))
add(s, list())
expect_equal(values(s), list(NULL, list()))
add(s, numeric(0))
expect_equal(values(s), list(NULL, list(), numeric(0)))


# Conversion
expect_equal(as.list(setnew(1, 2)), list(1, 2))

expect_equal(as.set(NULL), setnew())
expect_equal(as.set(container(1, 2, 1)), setnew(1, 2))

expect_equal(as.set(rep(1:3, 2)), setnew(1, 2, 3))
expect_equal(as.set(list(a = 1, a = 2, b = 1)), setnew(1, 2))

