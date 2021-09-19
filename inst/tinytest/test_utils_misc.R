exit_file(msg = "internal helper functions")

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

# ------------
# verify_names
# ------------
expect_error(verify_names(NULL))
expect_error(verify_names(""))
expect_error(verify_names(c(NA, NA)))
expect_true(verify_names("a"))


# --------------------
# check_name_collision
# --------------------
ee = expect_error
expect_true(check_name_collision("a", "b"))
ee(check_name_collision("a", "a"), "name 'a' exists already")
ee(check_name_collision(c("a", "b"), c("b", "c")), "name 'b' exists already")
ee(check_name_collision(c("a", "b"), c("b", "a")),
   "names 'a', 'b' exist already")

