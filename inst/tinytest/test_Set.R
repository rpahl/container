# ----------
# initialize
# ----------
s <- Set$new()
expect_true(s$empty())
expect_equal(s$length(), 0)
expect_false(s$has(NULL))
#expect_false(NULL %e% s) # TODO: %e% operator
expect_equal(attr(s, "class"), c("Set", "Container", "Iterable", "R6"))

expect_equal(Set$new(1, 1, 1), Set$new(1))
expect_equal(Set$new(NULL, NULL), Set$new(NULL))
expect_equal(Set$new(mean, mean, 1, 2), Set$new(mean, 1, 2))


# ---
# add
# ---
# adding zero-length elements works as expected
s <- Set$new(NULL)
expect_equal(s$length(), 1)
s$add(NULL) # is not added twice
expect_equal(s$length(), 1)
expect_equal(as.list(s$values()), list(NULL))

s$add(list())
expect_equal(as.list(s$values()), list(NULL, list()))
s$add(list())
expect_equal(as.list(s$values()), list(NULL, list()))

s$add(numeric(0))
expect_equal(as.list(s$values()), list(NULL, list(), numeric(0)))

# Output of zero-length elements looks as expected
out = utils::capture.output(s)
expect_equal(out[[1]], "<<Set(3)>> ")
expect_equal(out[[2]], "[<<NULL>>, list(), numeric()]")

# -----
# clear
# -----
s = Set$new(1, 2, 3)
expect_equal(s$clear(), Set$new())

# ------
# delete
# ------
co = Container$new(1, 2)
s2 = Set$new(3, 4)
s = Set$new(1, 2, 3, co, s2)
s$delete(1)
expect_equal(s, Set$new(2, 3, co, s2))
s$delete(co)
expect_equal(s, Set$new(2, 3, s2))
expect_error(s$delete(co))
s$delete(s2)
expect_equal(s, Set$new(2, 3))

# -------
# discard
# -------
co = Container$new(1, 2)
s2 = Set$new(3, 4)
s = Set$new(1, 2, 3, co, s2)
s$discard(1)
expect_equal(s, Set$new(2, 3, co, s2))
s$discard(co)
expect_equal(s, Set$new(2, 3, s2))
expect_silent(s$discard(co))
s$discard(s2)
expect_equal(s, Set$new(2, 3))

# -----
# empty
# -----
expect_true(Set$new()$empty())
expect_false(Set$new(NULL)$empty())
expect_false(Set$new(numeric())$empty())
expect_false(Set$new()$add(NULL)$empty())

# -----
# has
# -----
s = Set$new(1, "1", NULL, integer())
expect_true(s$has(1))
expect_true(s$has("1"))
expect_true(s$has(integer()))
expect_false(s$has(numeric()))
expect_false(s$has(1L))

co = Container$new(1, 2)
s$add(co)
expect_true(s$has(co))
co2 = Container$new(1, 2)
expect_false(s$has(co2))
co$add("a")
expect_true(s$has(co))

# --------
# peekitem
# --------
s = Set$new(1, 2, 3)
expect_true(s$peekitem() %in% 1:3)
expect_equal(s$length(), 3)
expect_true(is.null(Set$new()$peekitem()))

# -------
# popitem
# -------
s = Set$new(1, 2)
expect_true(s$popitem() %in% 1:2)
expect_equal(s$length(), 1)
expect_true(s$popitem() %in% 1:2)
expect_equal(s$length(), 0)
expect_error(s$popitem())


# ------
# values
# ------
s = Set$new(1, 2, 3)
expect_equal(s$values(), sets::set(1, 2, 3))

