exit_file(msg = "todo")

# ----
# Dict
# ----
# Dict [[<- operator
d = dict(a = 1, b = 2)
d[["a"]] <- "foo"
expect_equal(d$getval("a"), "foo")
expect_error(d[["z"]] <- 0, "key 'z' not in Dict")
expect_equal(values(d), list(a = "foo", b = 2)) # ensure d was not changed
expect_silent(d[["z", TRUE]] <- 0)
expect_equal(d$getval("z"), 0)

# Dict $<- operator
d = dict(a = 1, b = 2)
d$a <- "foo"
expect_equal(d$getval("a"), "foo")
expect_silent(d$z <- 0)
expect_equal(d$getval("z"), 0)

# Dict [<- operator
d = dict(a = 1, b = 2)
d[c("a", "b")] <- c(3, 4)
expect_equal(values(d), list(a = 3, b = 4))

d2 <- d$clone()
expect_equal(d, d2)
expect_error(d2[c("b", "x")] <- c(5, 6), "key 'x' not in Dict")
expect_equal(d, d2)  # ensure d2 was not changed (partially)

d2[c("b", "x"), add = TRUE] <- c(5, 6)
expect_equal(values(d2), list(a = 3, b = 5, x = 6))

d = dict(a = 1, b = 2)
d[c("a", "b")] <- 0
expect_equal(values(d), list(a = 0, b = 0))


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
expect_false("y" %e% s)
s[[]] <- 9
expect_true(9 %e% s)


# ----------
# dict.table
# ----------
# dict.table [[<- operator
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

expect_warning(dit[[1]] <- "a")
expect_true(all(is.na(dit[[1]])))
expect_silent(dit[["z", TRUE]] <- "a")
expect_equal(dit[["z"]], rep("a", 2))

# dict.table $<- operator
dit <- dict.table(a = 1:2, b = 2:1)
dit$a <- 3:4
expect_equal(dit$a, 3:4)
dit$x <- 0
expect_equal(dit$x, c(0, 0))

