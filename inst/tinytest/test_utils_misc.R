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


# ------------------
# .eval_range_select
# ------------------
local({
    f = container:::.eval_range_select
    vars = c("a", "b", "c", "d")

    # Standard cases based on non-standard evaluation (NSE)
    ee = expect_equal
    ee(f(vars, a), 1)
    ee(f(vars, b), 2)
    ee(f(vars, a:a), 1)
    ee(f(vars, b:b), 2)
    ee(f(vars, a:b), 1:2)
    ee(f(vars, b:a), 2:1)
    ee(f(vars, a:c), 1:3)
    ee(f(vars, b:d), 2:4)
    # Negative indices
    ee(f(vars, -a), -1)
    ee(f(vars, -a:c), -1:3)
    ee(f(vars, a:-c), 1:-3)
    ee(f(vars, -(a:c)), -(1:3))

    # Numeric indices
    ee(f(vars, 1), 1)
    ee(f(vars, 2), 2)
    ee(f(vars, 1:1), 1)
    ee(f(vars, 2:2), 2)
    ee(f(vars, 1:2), 1:2)
    ee(f(vars, 2:1), 2:1)
    ee(f(vars, 1:3), 1:3)
    ee(f(vars, 2:4), 2:4)
    # Negative indices
    ee(f(vars, -1), -1)
    ee(f(vars, -1:3), -1:3)
    ee(f(vars, 1:-3), 1:-3)
    ee(f(vars, -(1:3)), -(1:3))

    # Mixed indices
    ee(f(vars, 1:b), 1:2)
    ee(f(vars, a:2), 1:2)

    # Variables with empty spaces
    vars = c("a 1", "a 2", "b", "a 3")
    ee(f(vars, `a 1`:b), 1:3)
    ee(f(vars, `a 1`:3), 1:3)
    ee(f(vars, `a 1`:`a 3`), 1:4)
    # Negative indices

    # Works over empty variable names
    vars <- c("a", "", "b", "c")
    ee(f(vars, a:b), 1:3)

    # Invalid cases
    err = expect_error
    err(f(vars, a:z), "object 'z' not found")
    err(f(vars, z:b), "object 'z' not found")

    # Custom enclosing environment
    e <- new.env()
    e$z <- 3
    ee(f(vars, a:z, enclos = e), 1:3)
})


local({
    f <- unlist1

    # unlist1 unravels a list by one level
    l = list(a = list(x = list(1, 2)))

    expect_equal(as.numeric(unlist(l)), 1:2)
    expect_equal(f(l), list(a.x = list(1, 2)))
    expect_equal(f(l, use.names = FALSE), list(list(1, 2)))
})
