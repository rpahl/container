ee = expect_equal

# --------------
# get_at.Dict
# --------------
d = dict(a = 1, b = 2)
ee(get_at(d), dict())
ee(get_at(d, "a"), dict(a = 1))
ee(get_at(d, "a", "b"), d)

# Works also for duplicated keys
ee(get_at(d, "a", c("a", "b")), d)

expect_error(get_at(d, "x"), "key 'x' not in Dict")
expect_error(get_at(dict(), "x"), "key 'x' not in Dict")


# --------------------
# get_at.dict.table
# --------------------
d = dict.table(a = 1:2, b = 2:1)
ee(get_at(d), dict.table())
ee(get_at(d, "a"), dict.table(a = 1:2))
ee(get_at(d, "a", "b"), d)
ee(get_at(d, 1:2), d)
ee(get_at(d, 1, 2), d)

# Works also for duplicated keys
ee(get_at(d, "a", c("a", "b")), d)

expect_error(get_at(d, "a", 2), "column\\(s\\) not found: 2")
expect_error(get_at(d, "x"), "column\\(s\\) not found: x")
expect_error(get_at(d, 2:3), "index out of range \\(ncol = 2\\): 3")
expect_error(get_at(dict.table(), "x"), "column\\(s\\) not found: x")


