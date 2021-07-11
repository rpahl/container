# -------------
# .assert_index
# -------------
f = assert_index
co = container(1, 2, 3)

expect_error(f(co, 4), "index 4 exceeds length of Container, which is 3")
expect_error(f(1:3, 4), "index 4 exceeds length of numeric, which is 3")
expect_error(f(co, "a"), "index 'a' not found")
expect_error(f(co, 1:2), "index must be of length 1")
expect_error(f(co, NULL), "index must be of length 1")
expect_error(f(co, list(1)))
expect_error(f(co, index = NA), "index must not be 'NA'")


expect_true(f(co, 3))
expect_true(f(c(a = 1), "a"))
expect_true(f(list(a = 1), "a"))
expect_true(f(list(a = 1), "a"))


# ----------
# .has_index
# ----------
f = has_index
co = container(a = 1, 2)

expect_false(f(co, 0))
expect_true(f(co, 1))
expect_true(f(co, 2))
expect_false(f(co, 3))

expect_true(f(co, "a"))
expect_false(f(co, "b"))

expect_error(f(co, list(1)))
expect_error(f(co, index = 1:2), "index must be of length 1")
expect_error(f(co, index = NA), "index must not be 'NA'")


l = list(a = 1, 2)
expect_false(.has_index(l, ""))

