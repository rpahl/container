# ----------
# initialize
# ----------
s = Set$new()
expect_true(s$empty())
expect_equal(s$length(), 0)
expect_false(s$has(NULL))
#expect_false(NULL %e% s) # TODO: %e% operator
expect_equal(attr(s, "class"), c("Set", "Container", "Iterable", "R6"))

expect_equal(Set$new(1, 1, 1), Set$new(1))
expect_equal(Set$new(NULL, NULL), Set$new(NULL))
expect_equal(Set$new(mean, mean, 1, 2), Set$new(mean, 1, 2))

# Set elements can be named
s = Set$new(a = 1, b = 3)
expect_equal(names(s$values()), NULL)
s = Set$new(a = 1, b = 3, keep_names = TRUE)
expect_equal(names(s$values()), c("a", "b"))

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


# --------------
# Set operations
# --------------
s0   = Set$new()
s1   = Set$new(1)
s12  = Set$new(1, 2)
s23  = Set$new(   2, 3)
s1_3 = Set$new(1,    3)
s123 = Set$new(1, 2, 3)

original_sets_were_not_altered =
    isTRUE(all.equal(s0, Set$new()))            &&
    isTRUE(all.equal(s1, Set$new(1)))           &&
    isTRUE(all.equal(s12, Set$new(1, 2)))       &&
    isTRUE(all.equal(s23, Set$new(   2, 3)))    &&
    isTRUE(all.equal(s1_3, Set$new(1,   3)))    &&
    isTRUE(all.equal(s123, Set$new(1, 2, 3)))

# diff
expect_error(s1$diff(2))
expect_error(s1$diff(NULL))
expect_error(s1$diff(NA))

expect_equal(s0$diff(s1), s0)
expect_equal(s1$diff(s0), s1)
expect_equal(s123$diff(s0), s123)

expect_equal(s1$diff(s1), s0)
expect_equal(s1$diff(s12), s0)
expect_equal(s12$diff(s1), Set$new(2))
expect_equal(s123$diff(s1), s23)
expect_equal(s1_3$diff(s23), s1)

expect_true(original_sets_were_not_altered)

# intersect
expect_error(s1$intersect(2))
expect_error(s1$intersect(NULL))
expect_error(s1$intersect(NA))

expect_equal(s0$intersect(s0), s0)
expect_equal(s0$intersect(s123), s0)
expect_equal(s123$intersect(s0), s0)

expect_equal(s12$intersect(s23), s23$intersect(s12)) # commutativity
expect_equal(s1$intersect(s23), s0)
expect_equal(s12$intersect(s12), s12)

expect_true(original_sets_were_not_altered)

# union
expect_error(s1$union(2))
expect_error(s1$union(NULL))
expect_error(s1$union(NA))

expect_equal(s0$union(s0), s0)
expect_equal(s0$union(s123), s123)
expect_equal(s123$union(s0), s123)

expect_equal(s12$union(s23), s123)
expect_equal(s23$union(s12), s123)
expect_equal(s12$union(s1_3), s123)

expect_true(original_sets_were_not_altered)

# comparison
expect_error(s1$is_equal(2))
expect_error(s1$is_equal(NULL))
expect_error(s1$is_equal(NA))

expect_true(s0$is_equal(s0))
expect_true(s123$is_equal(s123))
expect_false(s0$is_equal(s1))
expect_false(s1$is_equal(s0))

expect_false(s0$is_subset(s0))
expect_true(s1$is_subset(s1))
expect_false(s1$is_subset(s0))
expect_true(s0$is_subset(s1))
expect_true(s1$is_subset(s12))
expect_false(s12$is_subset(s1))

expect_true(s0$is_proper_subset(s1))
expect_true(s1$is_proper_subset(s12))
expect_false(s0$is_proper_subset(s0))
expect_false(s1$is_proper_subset(s1))

expect_true(original_sets_were_not_altered)

