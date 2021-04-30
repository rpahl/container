ee = expect_equal

# -----------
# rename.Dict
# -----------
d <- dict(A = 1, B = 2)
expect_error(rename(d, 1, "C"), "'old' must be character")
expect_error(rename(d, "A", 1), "'new' must be character")
expect_error(rename(d, "A", c("C", "D")), "must be of the same length")
expect_error(rename(d, "A", "B"), "key 'B' already in Dict")
expect_error(rename(d, "Z", "B"), "Items of 'old' not found in names: Z")
expect_error(rename(d, c("A", "A"), c("a", "a")),
             "'old' has duplicated names: A")

vals = as.numeric(values(d))
rename(d, "A", "a")
expect_true(has(d, "a"))
expect_false(has(d, "A"))

# Verify that values did not change
expect_equal(vals, as.numeric(values(d)))

# Several keys at once
rename(d, c("a", "B"), c("x", "y"))
expect_equal(names(d), c("x", "y"))

# Renaming same key multiple times is not possible
expect_error(rename(d, c("x", "x2"), c("x2", "x3")),
             "Items of 'old' not found in names: x2")

# -----------------
# rename.dict.table
# -----------------
dit = dict.table(A = 1:2, B = 2:1)
rename(dit, "A", "X")
ee(colnames(dit), c("X", "B"))
rename(dit, c("X", "B"), c("y", "z"))
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

