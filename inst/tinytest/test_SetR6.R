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
expect_equal(as.list(s$values()), list(list(), NULL))
s$add(list())
expect_equal(as.list(s$values()), list(list(), NULL))

s$add(numeric(0))
expect_equal(as.list(s$values()), list(list(), NULL, numeric()))

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
expect_true(s$has(numeric()))
expect_true(s$has(1L))

# Use identical as the comparison function to achieve different result:
container_options(compare = identical)
s.ident = Set$new(s$values(), .cmp = identical)
expect_false(s.ident$has(numeric()))
expect_false(s.ident$has(1L))
container_options(.reset = TRUE)

# Membership of container objects depends on comparison function. By default
# (all.equal) a copy with the same elements is considered equal. Using
# 'identical' will check for the exact reference.
s  = Set$new()
container_options(compare = identical)
s.ident  = Set$new()
container_options(.reset = TRUE)

co = Container$new(1, 2)
s$add(co)
s.ident$add(co)
expect_true(s$has(co))
expect_true(s.ident$has(co))

co2 = Container$new(1, 2)
expect_true(s$has(co2))         # TRUE, because co2 has same elements as co
expect_false(s.ident$has(co2))  # FALSE, as the reference is checked

co$add("a")
expect_true(s$has(co))
expect_true(s.ident$has(co))


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

# -----
# print
# -----
out = capture.output(print(Set$new()))
expect_equal(out, "{}")

s = Set$new(1, 1L, NULL, integer())
out = capture.output(print(s))
expect_equal(out, "{integer(), NULL, 1}")
s = Set$new(1L, 1, NULL, integer())
out = capture.output(print(s))
expect_equal(out, "{integer(), NULL, 1L}")

# Using identical lets will keep both numeric and integer
container_options(compare = identical)
s = Set$new(1, 1L, NULL, integer())
out = capture.output(print(s))
expect_equal(out, "{integer(), NULL, 1, 1L}")
container_options(.reset = TRUE)

s2 = Set$new(list(), 3:5, s)
out = capture.output(print(s2))
expect_equal(out, "{list(), (3L 4L 5L), {integer(), NULL, 1, 1L}}")

# Increasing the size of the first Set alters the output
s$add(1)$add(2)$add(3)
out = capture.output(print(s2))
expect_equal(out, "{list(), (3L 4L 5L), <<Set(6)>>}")

s2$add(data.frame(A = 1:3, B = 3:1))
out = capture.output(print(s2))
expect_equal(out, "{list(), <<data.frame(3x2)>>, (3L 4L 5L), <<Set(6)>>}")


# -------
# replace
# -------
# Signals an error if elem does not exist unless add == TRUE
expect_error(Set$new()$replace(0, 1))
expect_error(Set$new()$replace(NULL, 1))
expect_error(Set$new(0)$replace(1, 2))

expect_equal(Set$new()$replace(0, 1, add = TRUE), Set$new(1))

# Replacing to an existing element, reduces the set
s = Set$new(1, 2)
s$replace(2, 1)
expect_equal(s, Set$new(1))
s$replace(1, NULL)
expect_equal(s, Set$new(NULL))

# Replacing to non-existing element works as expected
s = Set$new(1, 2, 3)
s$replace(1, 4)
expect_equal(s, Set$new(2, 3, 4))

s = Set$new(1, "1")
s$replace(1, 0)
expect_equal(s, Set$new(0, "1"))

# Replace works on special elements of basic type
s = Set$new(NULL, numeric(0), list())
s$replace(NULL, 0)
expect_equal(s, Set$new(0, numeric(), list()))
s$replace(numeric(0), 0)
expect_equal(s, Set$new(0, list()))
s$replace(list(), 0)
expect_equal(s, Set$new(0))

# Replace works on non-basic objects
S1 = Set$new(1, "1")
S2 = Set$new(2, "2")
Co = Container$new(NULL)
s = Set$new(S1, S2, Co)
s$replace(S1, 1)
expect_equal(s, Set$new(1, S2, Co))
s$replace(S2, 2)
expect_equal(s, Set$new(1, 2, Co))
s$replace(Co, 0)
expect_equal(s, Set$new(0, 1, 2))

# ------
# values
# ------
s = Set$new(1, 2, 3)
expect_equal(s$values(), list(1, 2, 3))

# -----
# clone
# -----
# Set objects provide reference semantics but can also be cloned
s1 <- Set$new(a = 1, 2, 3)
s2 <- s1
ss <- s1$clone()
expect_true(identical(s1, s2))
expect_false(identical(s1, ss))
expect_equal(s1$length(), ss$length())
expect_equal(s1, ss)

s1$delete(3)
expect_true(identical(s1, s2))
expect_true(s1$length() < ss$length())

# Set objects can be even cloned deeply
s1 = Set$new(a = 1)
ss.deep = s1$clone(deep = TRUE)
expect_equal(s1$values(), ss.deep$values())

s2 = Set$new(s1)
ss = s2$clone()
expect_equal(unpack(ss), c(a = 1))
s1$add(2)   # since it was not a deep clone, this will be modified in ss as well
expect_equal(unpack(ss), c(a = 1, 2))

ss.deep = s2$clone(deep = TRUE)
expect_equal(unpack(ss.deep), c(a = 1, 2))
s1$add(3)   # this again affects ss but not ss.deep
expect_equal(unpack(ss), c(a = 1, 2, 3))
expect_equal(unpack(ss.deep), c(a = 1, 2))

# Do one more nested layer
sss = Set$new(42, ss)
ss.deep = sss$clone(deep = TRUE)
s1$add(4)

expect_equal(unpack(sss), c(a = 1, 2:4, 42))
expect_equal(unpack(ss.deep), c(a = 1, 2:3, 42))

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

expect_equal(s0$diff(s1), s0)
expect_equal(s1$diff(s0), s1)
expect_equal(s123$diff(s0), s123)

expect_equal(Set$new(1)$diff(s1), s0)
expect_equal(Set$new(1)$diff(s12), s0)
expect_equal(Set$new(1, 2)$diff(s1), Set$new(2))
expect_equal(Set$new(1, 2, 3)$diff(s1), s23)
expect_equal(Set$new(1, 3)$diff(s23), s1)

# intersect
expect_error(s1$intersect(2), "arg must be a Set")
expect_error(s1$intersect(NULL), "arg must be a Set")
expect_error(s1$intersect(NA), "arg must be a Set")

expect_equal(s0$intersect(s0), s0)
expect_equal(s0$intersect(s123), s0)
expect_equal(Set$new(1, 2, 3)$intersect(s0), s0)

expect_equal(Set$new(1, 2)$intersect(Set$new(2, 3)),
             Set$new(2, 3)$intersect(Set$new(1, 2))) # commutativity
expect_equal(Set$new(1)$intersect(s23), s0)
expect_equal(Set$new(1, 2)$intersect(s12), s12)


# union
expect_error(s1$union(2), "arg must be a Set")
expect_error(s1$union(NULL), "arg must be a Set")
expect_error(s1$union(NA), "arg must be a Set")

expect_equal(s0$union(s0), s0)
expect_equal(Set$new()$union(s123), s123)
expect_equal(Set$new(1, 2, 3)$union(s0), s123)

expect_equal(Set$new(1, 2)$union(s23), s123)
expect_equal(Set$new(2, 3)$union(s12), s123)
expect_equal(Set$new(1, 2)$union(s1_3), s123)


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

