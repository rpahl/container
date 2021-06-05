# -------------
# .assert_index
# -------------
f = .assert_index

expect_error(f(1:3, 4), "index 4 exceeds length of numeric, which is 3")
expect_error(f(1:3, "a"), "index 'a' not found")
expect_error(f(1:3, 1:2), "index must be of length 1")
expect_error(f(1:3, NULL), "index must be of length 1")
expect_error(f(1:3, list(1)), "invalid index type 'list'")

expect_true(f(1:3, 3))
expect_true(f(c(a = 1), "a"))
expect_true(f(list(a = 1), "a"))
expect_true(f(list(a = 1), "a"))


