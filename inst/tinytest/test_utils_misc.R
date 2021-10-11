# ----------
# .is_string
# ----------
f = container:::.is_string
expect_false(f(1))
expect_false(f(NULL))
expect_false(f(NA))
expect_false(f(character()))
expect_false(f(as.character(NA)))
expect_false(f(c("a", "b")))
expect_true(f(""))
expect_true(f("a"))
expect_true(f("1"))

# -------------------
# .is_nonempty_string
# -------------------
f = container:::.is_nonempty_string
expect_false(f(1))
expect_false(f(NULL))
expect_false(f(NA))
expect_false(f(character()))
expect_false(f(as.character(NA)))
expect_false(f(c("a", "b")))
expect_false(f(""))
expect_true(f("a"))
expect_true(f("1"))

# -------------
# .verify_names
# -------------
f = container:::.verify_names
expect_error(f(NULL))
expect_error(f(""))
expect_error(f(c(NA, NA)))
expect_true(f("a"))


# ---------------------
# .check_name_collision
# ---------------------
ee = expect_error
f = container:::.check_name_collision
expect_true(f("a", "b"))
ee(f("a", "a"), "name 'a' exists already")
ee(f(c("a", "b"), c("b", "c")), "name 'b' exists already")
ee(f(c("a", "b"), c("b", "a")),
   "names 'a', 'b' exist already")

