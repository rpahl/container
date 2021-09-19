ee = expect_equal

# ----------
# initialize
# ----------
s = Set$new()
expect_true(s$is_empty())
ee(s$length(), 0)
expect_false(s$has(NULL))
#expect_false(NULL %e% s) # TODO: %e% operator
ee(attr(s, "class"), c("Set", "Container", "Iterable", "R6"))

ee(Set$new(1, 1, 1), Set$new(1))
ee(Set$new(NULL, NULL), Set$new(NULL))
ee(Set$new(mean, mean, 1, 2), Set$new(mean, 1, 2))

# Set elements can be named
s = Set$new(a = 1, b = 3)
ee(names(s$values()), c("a", "b"))

# ---
# add
# ---
# adding zero-length elements works as expected
s <- Set$new(NULL)
ee(s$length(), 1)
s$add(NULL) # is not added twice
ee(s$length(), 1)
ee(as.list(s$values()), list(NULL))

s$add(list())
expect_true(setequal(as.list(s$values()), list(list(), NULL)))
s$add(list())
expect_true(setequal(as.list(s$values()), list(list(), NULL)))

s$add(numeric(0))
expect_true(setequal(as.list(s$values()), list(list(), NULL, numeric())))

# --
# at
# --
s = Set$new(a = 1, 2, b = 3, 4)
ee(s$at(1), Set$new(a = 1))
ee(s$at(2), Set$new(2))
ee(s$at(c("a", "b")), Set$new(a = 1, b = 3))
ee(s$at(list(1, "b")), Set$new(a = 1, b = 3))

ee(s$at(1:2), Set$new(a = 1, 2))
ee(s$at("a"), s$at(match("a", names(s))))
ee(s$at("b"), s$at(match("b", names(s))))

expect_error(s$at(0), "index must be > 0")
expect_error(s$at(-1), "index must be > 0")
expect_error(s$at("c"), "index 'c' not found")
expect_error(s$at(as.numeric(NA)), "index must not be 'NA'")

s = Set$new(a = 10, b = 1)
ee(s$at(1), Set$new(b = 1))

# ---
# at2
# ---
s = Set$new(a = 1, 2, b = 3, 4)
ee(s$at2(1), 1)
ee(s$at2(2), 2)
ee(s$at2("a"), 1)

expect_error(s$at2(1:2), "index must be of length 1")
expect_error(s$at2(0), "index must be > 0")
expect_error(s$at2(-1), "index must be > 0")
expect_error(s$at2(5), "index 5 exceeds length of Set, which is 4")
expect_error(s$at2(as.numeric(NA)), "index must not be 'NA'")
expect_error(s$at2(c("a", "b")), "index must be of length 1")
expect_error(s$at2("c"), "index 'c' not found")


# -----
# clear
# -----
s = Set$new(1, 2, 3)
ee(s$clear(), Set$new())

# ------
# delete
# ------
co = Container$new(1, 2)
s2 = Set$new(3, 4)
s = Set$new(1, 2, 3, co, s2)
s$delete(1)
ee(s, Set$new(2, 3, co, s2))
s$delete(co)
ee(s, Set$new(2, 3, s2))
expect_error(s$delete(co))
s$delete(s2)
ee(s, Set$new(2, 3))

# -------
# discard
# -------
co = Container$new(1, 2)
s2 = Set$new(3, 4)
s = Set$new(1, 2, 3, co, s2)
s$discard(1)
ee(s, Set$new(2, 3, co, s2))
s$discard(co)
ee(s, Set$new(2, 3, s2))
expect_silent(s$discard(co))
s$discard(s2)
ee(s, Set$new(2, 3))

# ----------
# discard_at
# ----------
co = Container$new(1, 2)
s2 = Set$new(3, 4)
s = Set$new(1, 2, 3, co = co, s = s2)
ee(s$discard_at(1), Set$new(2, 3, co = co, s = s2))

ee(s$discard_at("co"), Set$new(2, 3, s = s2))
expect_silent(s$discard_at("co"))
ee(s$discard_at("s")$values(), list(2, 3))


# -----
# empty
# -----
expect_true(Set$new()$is_empty())
expect_false(Set$new(NULL)$is_empty())
expect_false(Set$new(numeric())$is_empty())
expect_false(Set$new()$add(NULL)$is_empty())

# -----
# has
# -----
s = Set$new(1, "1", NULL, integer())
expect_true(s$has(1))
expect_true(s$has("1"))
expect_true(s$has(integer()))
expect_true(s$has(numeric()))
expect_true(s$has(1L))

# Use identical as the comparison function to achieve different result:
container_options(compare = identical)
si = Set$new(s$values(), .cmp = identical)
expect_false(si$has(numeric()))
expect_false(si$has(1L))

si = Set$new(1, 1L)
expect_true(setequal(as.list(si), list(1.0, 1L)))
container_options(.reset = TRUE)

# Membership of container objects depends on comparison function. By default
# (all.equal) a copy with the same elements is considered equal. Using
# 'identical' will check for the exact reference.
s  = Set$new()
container_options(compare = identical)
si  = Set$new()
container_options(.reset = TRUE)

co = Container$new(1, 2)
s$add(co)
si$add(co)
expect_true(s$has(co))
expect_true(si$has(co))

co2 = Container$new(1, 2)
expect_true(s$has(co2))         # TRUE, because co2 has same elements as co
expect_false(si$has(co2))       # FALSE, as the reference is checked

co$add("a")
expect_true(s$has(co))
expect_true(si$has(co))


# -------
# peek_at
# -------
s = Set$new(a = 1, 2, b = 3, 4)
ee(s$peek_at(1), Set$new(a = 1))
ee(s$peek_at(2), Set$new(2))
ee(s$peek_at(c("a", "b")), Set$new(a = 1, b = 3))
ee(s$peek_at(list(1, "b")), Set$new(a = 1, b = 3))
ee(s$peek_at(), s)

ee(s$peek_at(c(1, 1)), Set$new(a = 1, a = 1))
ee(s$peek_at(c("a", "a")), Set$new(a = 1, a = 1))

ee(s$peek_at(1:2), Set$new(a = 1, 2))
ee(s$peek_at("a"), s$peek_at(match("a", names(s))))
ee(s$peek_at("b"), s$peek_at(match("b", names(s))))

ee(s$peek_at(0), Set$new())
ee(s$peek_at(-1), Set$new())
ee(s$peek_at("c"), Set$new())
ee(s$peek_at(as.numeric(NA)), Set$new())
ee(s$peek_at(0, default = "foo"), Set$new("foo"))
ee(s$peek_at("z", default = "foo"), Set$new(z = "foo"))

ee(s$peek_at(list("a", "x", 9), default = 0), Set$new(a = 1, x = 0, 0))
ee(s$peek_at(c("a", "x", 9), default = 0), Set$new(a = 1, x = 0, "9" = 0))
ee(s$peek_at(c(NA, NA), default = 0), Set$new(0, 0))
ee(s$peek_at(NULL), Set$new())
ee(s$peek_at(NULL, default = 0), Set$new())
ee(s$peek_at(list(a = NULL), default = 0), Set$new())
ee(s$peek_at(c(NULL, NA), default = 0), Set$new(0))
ee(s$peek_at(c(NA, NULL), default = 0), Set$new(0))

ee(s$peek_at(list("s1" = "a", "s2" = "x", "s3" = NULL), default = 0),
   Set$new(a = 1, x = 0))


# --------
# peek_at2
# --------
s = Set$new(a = 1, 2, b = 3, 4)
ee(s$peek_at2(1), 1)
ee(s$peek_at2(2), 2)
ee(s$peek_at2("a"), 1)
ee(Set$new()$peek_at2(1), NULL)
ee(Set$new()$peek_at2(1, default = 0), 0)
ee(s$peek_at2(), NULL)

expect_error(s$peek_at2(1:2), "index must be of length 1")
expect_error(s$peek_at2(c("a", "b")), "index must be of length 1")
expect_error(s$peek_at2(NA), "index must not be 'NA'")

ee(s$peek_at2(0), NULL)
ee(s$peek_at2(0, default = "foo"), "foo")
ee(s$peek_at2(-1), NULL)
ee(s$peek_at2(-1, default = "foo"), "foo")
ee(s$peek_at2(5, default = 0), 0)
ee(s$peek_at2("c", 0), 0)


# ---
# pop
# ---
s = setnew(a = 1, 2, b = 3, 4)

ee(s$pop("a"), 1)
ee(s, setnew(2, b = 3, 4))

ee(s$pop(1), 2)
ee(s, setnew(b = 3, 4))

ee(s$pop(), 4)
ee(s, setnew(b = 3))

expect_error(s$pop(0), "index must be > 0")
expect_error(s$pop(3), "index 3 exceeds length of Set, which is 1")

ee(s$pop(), 3)
ee(s, setnew())

expect_error(s$pop(), "pop at empty Set")


# -----
# print
# -----
out = capture.output(print(Set$new()))
ee(out, "{}")

s = Set$new(1, 2, 3)
out = capture.output(print(s))
ee(out, "{1, 2, 3}")


# -------
# replace
# -------
# Signals an error if elem does not exist unless add == TRUE
s = Set$new()
expect_error(s$replace(0, 1), "old element \\(0\\) is not in Set")
expect_error(s$replace(NULL, 1), "old element \\(NULL\\) is not in Set")
expect_error(s$add(0)$replace(1, 2), "old element \\(1\\) is not in Set")
ee(Set$new()$replace(0, 1, add = TRUE), Set$new(1))

# Replacing to an existing element, reduces the set
s = Set$new(1, 2)
ee(s$replace(2, 1), Set$new(1))
ee(s$replace(1, NULL), Set$new(NULL))

# Replacing a named element preserves the name
s = Set$new(a = 1, b = 2)
ee(s$replace(1, 0), Set$new(a = 0, b = 2))

# Replacing by new element works as expected
s = Set$new(a = 1, 2, 3)
ee(s$replace(1, 4), Set$new(2, 3, a = 4))

s = Set$new(1, "1")
ee(s$replace(1, 0), Set$new(0, "1"))

# Replace works on special elements of basic type
s = Set$new(NULL, numeric(0), list())
ee(s$replace(NULL, 0), Set$new(0, numeric(), list()))
ee(s$replace(numeric(0), 0), Set$new(0, list()))
ee(s$replace(list(), 0), Set$new(0))

# Replace works on non-basic objects
S1 = Set$new(1, "1")
S2 = Set$new(2, "2")
Co = Container$new(NULL)
s = Set$new(S1, S2, Co)
ee(s$replace(S1, 1), Set$new(1, S2, Co))
ee(s$replace(S2, 2), Set$new(1, 2, Co))
ee(s$replace(Co, 0), Set$new(0, 1, 2))

# ------
# rename
# ------
x <- Set$new(A = 1, B = 2, 3, D = 4)

vals = as.numeric(x$values())
x$rename("A", "a")
expect_true(x$has_name("a"))
expect_false(x$has_name("A"))

# Verify that values did not change
values_did_not_change = all.equal(vals, as.numeric(x$values()))
expect_true(values_did_not_change)

# Several keys at once
x$rename(c("a", "B"), c("x", "y"))
ee(names(x), c("x", "y", "", "D"))

x$rename("D", "4")
ee(names(x), c("x", "y", "", "4"))


# ----------
# replace_at
# ----------
# Signals an error for invalid index unless add == TRUE
s = Set$new()
expect_error(s$replace_at(1, 1), "index 1 exceeds length of Set, which is 0")
expect_error(s$replace_at(NULL, 1), "index must be of length 1")
ee(Set$new()$replace_at(0, 1, add = TRUE), Set$new(1))

# Replacing to an existing element, reduces the set
s = Set$new(1, 2)
ee(s$replace_at(2, 1), Set$new(1))
ee(s$replace_at(1, NULL), Set$new(NULL))

# Replacing a named element preserves the name
s = Set$new(a = 1, b = 2)
ee(s$replace_at(1, 0), Set$new(a = 0, b = 2))

# Replacing by new element works as expected
s = Set$new(a = 1, 2, 3)
ee(s$replace_at(1, 4), Set$new(2, 3, a = 4))
ee(s$replace_at("a", 5), Set$new(2, 3, a = 5))

# Replace works on special elements of basic type
s = Set$new(list(), NULL, numeric(0))
ee(Set$new(list())$replace_at(1, 0), Set$new(0))
ee(Set$new(numeric())$replace_at(1, 0), Set$new(0))
ee(Set$new(NULL)$replace_at(1, 0), Set$new(0))

# Replace works on non-basic objects
S1 = Set$new(1, "1")
S2 = Set$new(2, "2")
Co = Container$new(NULL)
s = Set$new(co = Co, s1 = S1, s2 = S2)
ee(s$replace_at(1, 1), Set$new(co = 1, s1 = S1, s2 = S2))


# ------
# update
# ------
# a Set can be updated by another Set object
s0 <- Set$new(A = 0)
s1 <- Set$new(A = 1, B = 2, C = 12)
s2 <- Set$new(              C = 3, D = 4)

ee(s0$update(s0),        Set$new(A = 0))
ee(s0$update(Set$new()), Set$new(A = 0))
ee(Set$new()$update(s0), Set$new(A = 0))

ee(s1$update(Set$new()), s1)
ee(s1$update(s2)$values(), list(A = 1, B = 2, C = 3, D = 4))
ee(Set$new()$update(s2), s2)

# a Set can be updated by another Container object with partially unnamed elements
ee(Set$new(a = 0)$update(Container$new(2, a = 1, 1)),
   Set$new(a = 1, 2))

ee(Set$new(a = 0)$update(Container$new(2, a = 1, 1, b = 2, x = 5, a = 3)),
   Set$new(a = 3, 2, x = 5))


# ------
# values
# ------
s = Set$new(1, 2, 3)
ee(s$values(), list(1, 2, 3))

# -----
# clone
# -----
# Set objects provide reference semantics but can also be cloned
s1 <- Set$new(a = 1, 2, 3)
s2 <- s1
ss <- s1$clone()
expect_true(identical(s1, s2))
expect_false(identical(s1, ss))
ee(s1$length(), ss$length())
ee(s1, ss)

s1$delete(3)
expect_true(identical(s1, s2))
expect_true(s1$length() < ss$length())

# Set objects can be even cloned deeply
s1 = Set$new(a = 1)
ss.deep = s1$clone(deep = TRUE)
ee(s1$values(), ss.deep$values())

s2 = Set$new(s1)
ss = s2$clone()
ee(unpack(ss), c(a = 1))
s1$add(2)   # since it was not a deep clone, this will be modified in ss as well
ee(unpack(ss), c(a = 1, 2))

ss.deep = s2$clone(deep = TRUE)
ee(unpack(ss.deep), c(a = 1, 2))
s1$add(3)   # this again affects ss but not ss.deep
ee(unpack(ss), c(a = 1, 2, 3))
ee(unpack(ss.deep), c(a = 1, 2))

# Do one more nested layer
sss = Set$new(42, ss)
ss.deep = sss$clone(deep = TRUE)
s1$add(4)

expect_true(setequal(as.list(unpack(sss)), list(a = 1, 2, 3, 4, 42)))
expect_true(setequal(as.list(unpack(ss.deep)), list(a = 1, 2, 3, 42)))

# --------------
# Set operations
# --------------
s0   = Set$new()
s1   = Set$new(1)
s12  = Set$new(1, 2)
s23  = Set$new(   2, 3)
s1_3 = Set$new(1,    3)
s123 = Set$new(1, 2, 3)

# diff
expect_error(s1$diff(2), "arg must be a Set")
expect_error(s1$diff(NULL), "arg must be a Set")
expect_error(s1$diff(NA), "arg must be a Set")

ee(s0$diff(s1), s0)
ee(s1$diff(s0), s1)
ee(s123$diff(s0), s123)

ee(Set$new(1)$diff(s1), s0)
ee(Set$new(1)$diff(s12), s0)
ee(Set$new(1, 2)$diff(s1), Set$new(2))
ee(Set$new(1, 2, 3)$diff(s1), s23)
ee(Set$new(1, 3)$diff(s23), s1)

# intersect
expect_error(s1$intersect(2), "arg must be a Set")
expect_error(s1$intersect(NULL), "arg must be a Set")
expect_error(s1$intersect(NA), "arg must be a Set")

ee(s0$intersect(s0), s0)
ee(s0$intersect(s123), s0)
ee(Set$new(1, 2, 3)$intersect(s0), s0)

ee(Set$new(1, 2)$intersect(Set$new(2, 3)),
             Set$new(2, 3)$intersect(Set$new(1, 2))) # commutativity
ee(Set$new(1)$intersect(s23), s0)
ee(Set$new(1, 2)$intersect(s12), s12)


# union
expect_error(s1$union(2), "arg must be a Set")
expect_error(s1$union(NULL), "arg must be a Set")
expect_error(s1$union(NA), "arg must be a Set")

ee(s0$union(s0), s0)
ee(Set$new()$union(s123), s123)
ee(Set$new(1, 2, 3)$union(s0), s123)

ee(Set$new(1, 2)$union(s23), s123)
ee(Set$new(2, 3)$union(s12), s123)
ee(Set$new(1, 2)$union(s1_3), s123)


# comparison
expect_error(s1$is_equal(2))
expect_error(s1$is_equal(NULL))
expect_error(s1$is_equal(NA))

expect_true(s0$is_equal(s0))
expect_true(s123$is_equal(s123))
expect_false(s0$is_equal(s1))
expect_false(s1$is_equal(s0))

expect_true(s0$is_subset(s0))
expect_true(s1$is_subset(s1))
expect_false(s1$is_subset(s0))
expect_true(s0$is_subset(s1))
expect_true(s1$is_subset(s12))
expect_false(s12$is_subset(s1))

expect_true(s0$is_proper_subset(s1))
expect_true(s1$is_proper_subset(s12))
expect_false(s0$is_proper_subset(s0))
expect_false(s1$is_proper_subset(s1))

