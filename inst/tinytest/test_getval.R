ee = expect_equal

# -----------
# getval.Dict
# -----------
d = dict(a = 1, b = 2)
ee(getval(d, "a"), 1)
ee(getval(d, c("a", "b")), d)

# Works also for duplicated keys
ee(getval(d, c("a", "b", "a")), d)

expect_error(getval(d, "x"), "key 'x' not in Dict")
expect_error(getval(dict(), "x"), "key 'x' not in Dict")


# -----------------
# getval.dict.table
# -----------------
d = dict.table(a = 1:2, b = 2:1)
ee(getval(d, "a"), 1:2)
ee(getval(d, 1), 1:2)
ee(getval(d, c("a", "b")), d)
ee(getval(d, 1:2), d)

# Works also for duplicated keys
ee(getval(d, c("a", "b", "a")), d)
ee(getval(d, c(1, 2, 1, 2)), d)

expect_error(getval(d, "x"), "column 'x' not in dict.table")
expect_error(getval(dict.table(), "x"), "column 'x' not in dict.table")


