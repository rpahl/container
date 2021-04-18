ee = expect_equal

# --------------
# getvalues.Dict
# --------------
d = dict(a = 1, b = 2)
ee(getvalues(d, "a"), 1)
ee(getvalues(d, "a", "b"), d)

# Works also for duplicated keys
ee(getvalues(d, "a", c("a", "b")), d)

expect_error(getvalues(d, "x"), "key 'x' not in Dict")
expect_error(getvalues(dict(), "x"), "key 'x' not in Dict")

# -----------------
# getval.dict.table
# -----------------
d = dict.table(a = 1:2, b = 2:1)
ee(getvalues(d, "a"), 1:2)
ee(getvalues(d, "a", "b"), d)

# Works also for duplicated keys
ee(getvalues(d, "a", c("a", "b")), d)

expect_error(getvalues(d, "x"), "column 'x' not in dict.table")
expect_error(getvalues(dict.table(), "x"), "column 'x' not in dict.table")


