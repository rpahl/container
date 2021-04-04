# ---------
# is_string
# ---------
f = is_string
expect_false(f(1))
expect_false(f(NULL))
expect_false(f(NA))
expect_false(f(character()))
expect_false(f(as.character(NA)))
expect_false(f(c("a", "b")))
expect_true(f(""))
expect_true(f("a"))
expect_true(f("1"))

# ------------------
# is_nonempty_string
# ------------------
f = is_nonempty_string
expect_false(f(1))
expect_false(f(NULL))
expect_false(f(NA))
expect_false(f(character()))
expect_false(f(as.character(NA)))
expect_false(f(c("a", "b")))
expect_false(f(""))
expect_true(f("a"))
expect_true(f("1"))

