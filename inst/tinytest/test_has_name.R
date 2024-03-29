ee = expect_equal

# ------------------
# has_name.Container
# ------------------
co = container(a = 1, 2, f = mean)
expect_true(has_name(co))
expect_false(has_name(container()))
expect_true(has_name(co, "a"))
expect_true(has_name(co, "f"))
expect_false(has_name(co, "2"))


# -------------------
# has_name.dict.table
# -------------------
dit = dict.table(a = 1:2, b = 3:4)

expect_true(has_name(dit, "a"))
expect_false(has_name(dit, "x"))

EE = expect_error
EE(has_name(dit, NULL), "name must be a character string, but got 'NULL'")
EE(has_name(dit, c("a", "b")), "name must be of length 1")
EE(has_name(dit, as.character(NA)), "undefined name")
EE(has_name(dit, ""), "name must consist of at least one character")

