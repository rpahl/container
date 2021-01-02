
# Dict replacement operator
d = dict(a = 1, b = 2)
d[c("a", "b")] <- c(3, 4)
expect_equal(values(d), list(a = 3, b = 4))

d2 <- d$clone()
expect_equal(d, d2)
expect_error(d2[c("b", "x")] <- c(5, 6), "key 'x' not in Dict")
expect_equal(d, d2)  # ensure d2 was not changed (partially)

d2[c("b", "x"), add = TRUE] <- c(5, 6)
expect_equal(values(d2), list(a = 3, b = 5, x = 6))

d = dict(c(a = 1, b = 2))
d[c("a", "b")] <- 0
expect_equal(values(d), c(a = 0, b = 0))


# Set replacement operator
s = setnew(1L, "a", mean)
expect_true(s$has(mean))
s[[mean]] <- identity
expect_false(s$has(mean))
expect_true(s$has(identity))
s[[NULL]] <- median
expect_true(s$has(median))
expect_equal(length(s), 4)
expect_error(s[["x"]] <- "y", "'x' not found")
expect_true(has(s, "y"))

