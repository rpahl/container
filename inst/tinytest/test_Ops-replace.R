
# ----
# Dict
# ----
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


# ---
# Set
# ---
s = setnew(1L, "a", mean)
expect_true(s$has(mean))
s[[mean]] <- identity
expect_false(s$has(mean))
expect_true(s$has(identity))
s[[NULL]] <- median
expect_true(s$has(median))
expect_equal(length(s), 4)
expect_error(s[["x"]] <- "y", "'x' not found")
expect_true("y" %e% s)

# ----------
# dict.table
# ----------
dit <- dict.table(a = 1:2, b = 2:1)
expect_error(dit[["x"]] <- 3:4, "column 'x' not in dict.table")
expect_silent(dit[["x", TRUE]] <- 3:4)
expect_equal(dit[["x"]], 3:4)
dit[["a"]] <- 0
expect_equal(dit[["a"]], c(0, 0))

dit[["a"]] <- NULL
expect_false("a" %in% colnames(dit))
dit[["a", TRUE]] <- 1
expect_equal(dit[["a"]], c(1, 1))
expect_error(dit[["a"]] <- 1:3)
expect_equal(ncol(dit), 3)
expect_error(dit[[4]] <- 0, "4 is outside range")

dit[[1]] <- 9:10
expect_equal(dit[[1]], 9:10)
dit[[1]] <- 0
expect_equal(dit[[1]], c(0, 0))

