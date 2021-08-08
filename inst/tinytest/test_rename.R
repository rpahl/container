ee = expect_equal

# -----------
# rename.Dict
# -----------
d <- dict(A = 1, B = 2)
expect_error(rename(d, 1, "C"), "'old' must be character")
expect_error(rename(d, "A", 1), "'new' must be character")
expect_error(rename(d, "A", c("C", "D")), "must be of the same length")
expect_error(rename(d, "A", "B"), "name 'B' already in Dict")
expect_error(rename(d, "Z", "B"), "Items of 'old' not found in names: Z")
expect_error(rename(d, c("A", "A"), c("a", "a")),
             "'old' has duplicated names: A")

vals = as.numeric(as.list(d))
ee(rename(d, "A", "a"), dict(a = 1, B = 2))
ee(d, dict(A = 1, B = 2))   # names were changed by value
ref_rename(d, "A", "a")
ee(d, dict(a = 1, B = 2))   # now names were changed by reference

expect_true(has_name(d, "a"))
expect_false(has_name(d, "A"))

# Verify that values did not change
expect_equal(vals, as.numeric(as.list(d)))

# Several keys at once
ee(rename(d, c("a", "B"), c("x", "y")),dict(x = 1, y = 2))

# Renaming same key multiple times is not possible
d = dict(x = 1, y = 2)
expect_error(rename(d, c("x", "x2"), c("x2", "x3")),
             "Items of 'old' not found in names: x2")

# -----------------
# rename.dict.table
# -----------------
dit = dict.table(A = 1:2, B = 2:1)
ee(rename(dit, "A", "X"), dict.table(X = 1:2, B = 2:1))
ee(dit, dict.table(A = 1:2, B = 2:1))
ref_rename(dit, "A", "X")

ee(colnames(dit), c("X", "B"))
ref_rename(dit, c("X", "B"), c("y", "z"))
ee(colnames(dit), c("y", "z"))
expect_error(rename(dit, "A", "b"))

# Multiple renames
expect_error(rename(dit, c("y", "y2"), c("y2", "y3")),
             "Items of 'old' not found in names: y2")

# --------------
# rename.default
# --------------
v <- c(a = 1, b = 2)
ee(rename(v, "a", "a1"), c(a1 = 1, b = 2))
ee(rename(v, c("a", "b"), c("a1", "b1")), c(a1 = 1, b1 = 2))

