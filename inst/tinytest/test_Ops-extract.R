exit_file(msg = "todo")
ee = expect_equal

# ----
# Dict
# ----
# Dict [[ operator
d = dict(a = 1, b = 2)
ee(d[["a"]], 1)
expect_error(d[["x"]], "key 'x' not in Dict")
expect_error(d[[c("a", "b")]], "key must be of length 1")
ee(d[["x", 9]], 9)

# Dict [ operator
d = dict(a = 1, b = 2, c = "z")
ee(d["a"], dict(a = 1))
ee(d[c("a", "c")], dict(a = 1, c = "z"))
expect_error(d["x"], "key 'x' not in Dict")
expect_error(d[c("a", "x", "c")], "key 'x' not in Dict")


# ----------
# dict.table
# ----------
# dict.table [[-operator
dit <- dict.table(a = 1:10, b = 10:1)
ee(dit[["a"]], 1:10)
ee(dit[[1]], 1:10)
ee(dit[[1.1]], 1:10)
ee(dit[["b"]], 10:1)
expect_error(dit[[c("a", "b")]], "column index must be of length 1")
expect_error(dit[[1:2]], "column index must be of length 1")
ee(dit[["a"]], dit[[1]])
expect_error(dit[["x"]], "column 'x' not in dict.table")
ee(dit[["x", 0]], rep(0, nrow(dit)))
ee(dit[["x", list(mean)]], rep(list(mean), nrow(dit)))
ee(dit[["x", 0]], dit[[3, 0]])
expect_warning(dit[["x", 1:2]])
suppressWarnings(ee(dit[["x", 1:2]], rep(1:2, nrow(dit)/2)))

# dict.table $-operator
dit <- dict.table(alpha = 1:10, beta = 10:1)
ee(dit$a, 1:10)
expect_error(dit$x, "column 'x' not in dict.table")

