ee = expect_equal
EE = expect_error

# ----------
# initialize
# ----------
# Dict constructor works as expected
d = Dict$new()

ee(attr(d, "class"), c("Dict", "Container", "Iterable", "R6"))
ee(mode(d$values()), "list")

d <- Dict$new(env = environment())
ee(d$length(), 1)

d <- Dict$new(env = environment(), foo = identity)
ee(d$length(), 2)

expect_true(Dict$new()$is_empty())

expect_error(Dict$new(1:2), "all elements must be named")
expect_error(Dict$new(x = 1, y = 2, x = 3), "duplicated keys")
ee(Dict$new()$keys(), character(0))

# One element
ee(Dict$new(x = 1)$values(), list(x = 1))
ee(Dict$new(x = NULL)$values(), list(x = NULL))
ee(Dict$new(x = 1:4)$values(), list(x = 1:4))
env = new.env()
ee(Dict$new(x = env)$values(), list(x = env))

# Two (or more) elements
d <- Dict$new(x = 1:2, y = 2:3)
ee(d$values(), Container$new(x = 1:2, y = 2:3)$values())
ee(names(d$values()), c("x", "y"))

# a Dict's keys are always sorted
v <- c(h = 1, d = 2, a = 8, b = 0)
d <- as.dict(v)
ee(d$keys(), sort(names(v)))


# ---
# add
# ---
# adding elements to a Dict requires a character key and a value
d <- Dict$new()
EE(d$add(1), 'argument "value" is missing')
EE(d$add(name = "a"), 'argument "value" is missing')
EE(d$add("", 1), "name must consist of at least one character")
EE(d$add(1, 1), "name must be a character string, but got 'numeric'")
EE(d$add(c("a", "b"), 1:2), "name must be of length 1")

# added elements must have distinct keys and cannot be added twice
d <- Dict$new()
ee(d$add("a", 1), Dict$new(a = 1))
EE(d$add("a", 1), "name 'a' already in Dict")
EE(d$add("a", 2), "name 'a' already in Dict")
EE(d$add("a", NULL), "name 'a' already in Dict")

# NULL and empty lists can be added
d <- Dict$new()
d$add("empty-list", list())
d$add("null", NULL)
ee(d$values(), list("empty-list" = list(), "null" = NULL))

# --
# at
# --
d = Dict$new(a = 1, b = 3)
ee(d$at(1), Dict$new(a = 1))
ee(d$at(2), Dict$new(b = 3))
ee(d$at(c("a", "b")), Dict$new(a = 1, b = 3))
ee(d$at(list(1, "b")), Dict$new(a = 1, b = 3))

expect_error(d$at(c(1, 1)), "duplicated keys are not allowed")

ee(d$at(1:2), Dict$new(a = 1, b = 3))
ee(d$at("a"), d$at(match("a", names(d))))
ee(d$at("b"), d$at(match("b", names(d))))

expect_error(d$at(0), "index must be > 0")
expect_error(d$at(-1), "index must be > 0")
expect_error(d$at("c"), "index 'c' not found")
expect_error(d$at(as.numeric(NA)), "index must not be 'NA'")

d = Dict$new(x = 1, b = 3)
ee(d$at(1), Dict$new(b = 3))
ee(d$at(2), Dict$new(x = 1))

# ---
# at2
# ---
d = Dict$new(a = 1, b = 3)
ee(d$at2(1), 1)
ee(d$at2(2), 3)
ee(d$at2("a"), 1)

expect_error(d$at2(1:2), "index must be of length 1")
expect_error(d$at2(0), "index must be > 0")
expect_error(d$at2(-1), "index must be > 0")
expect_error(d$at2(5), "index 5 exceeds length of Dict, which is 2")
expect_error(d$at2(as.numeric(NA)), "index must not be 'NA'")
expect_error(d$at2(c("a", "b")), "index must be of length 1")
expect_error(d$at2("c"), "index 'c' not found")


# ------
# delete
# ------
# elements can be deleted from a Dict
d <- Dict$new(a = 1, b = 2, c = 3)
ee(d$delete(3), Dict$new(a = 1, b = 2))

d <- Dict$new(a = mean, b = identity)
ee(d$delete(mean), Dict$new(b = identity))
expect_error(d$delete(), 'argument "elem" is missing, with no default')

# Dict gives an error if trying to delete non-existing element
d <- Dict$new(a = 1)
expect_error(d$delete(5), "5 is not in Dict")
li = list(1, 2)
expect_error(d$delete(li), "list\\(1, 2\\) is not in Dict")

# If duplicates, only one element is deleted
d <- Dict$new(a = 1, b = 2, c = 1)
ee(d$delete(1), Dict$new(b = 2, c = 1))


# ---------
# delete_at
# ---------
d <- Dict$new(a = 1)
expect_true(d$delete_at("a")$is_empty())

# if key not in Dict, trying to delete it gives an error
d <- Dict$new(a = 1, b = 2)
expect_error(Dict$new(a = 1)$delete_at("x"), "index 'x' not found")

# only one key can be deleted at a time
expect_error(d$delete_at(c("a", "b")), "index must be of length 1")

# failed delete does not alter the dict object
d_was_not_touched <- d$length() == 2
expect_true(d_was_not_touched)



# -------
# discard
# -------
ee(Dict$new()$discard(1), Dict$new())

# elements can be discarded from a Dict
d <- Dict$new(a = 1, b = 2, c = 3)
ee(d$discard(3), Dict$new(a = 1, b = 2))
ee(d$discard(1), Dict$new(b = 2))

d <- Dict$new(a = mean, b = identity)
ee(d$discard(mean), Dict$new(b = identity))
expect_error(d$discard(), 'argument "elem" is missing, with no default')

# Dict is not changed when trying to discard non-existing element
d <- Dict$new(a = 1)
expect_silent(ee(d$discard(5), d))


# ----------
# discard_at
# ----------
d = Dict$new()
ee(Dict$new()$discard_at(1), Dict$new())

# elements can be discarded from a Dict
d <- Dict$new(a = 1, b = 2, c = 3)
ee(d$discard_at(3), Dict$new(a = 1, b = 2))
ee(d$discard_at("a"), Dict$new(b = 2))

d <- Dict$new(a = mean, b = identity)
ee(d$discard_at(1), Dict$new(b = identity))
expect_error(d$discard_at(), "'index' is missing")

# Dict is not changed when trying to discard non-existing element
d <- Dict$new(a = 1)
expect_silent(ee(d$discard_at(5), d))
expect_silent(ee(d$discard_at(0), d))
expect_silent(ee(d$discard_at("x"), d))


# ---
# has
# ---
# it can be checked if Dict has a certain key
d <- Dict$new(a = 1, b = 2)
expect_true(d$has_name("a"))
expect_false(d$has_name("x"))
expect_error(d$has_name(c("a", "b")), "name must be of length 1")

# ----
# keys
# ----
# all keys can be listed
d <- Dict$new(a = 1, b = 2)
ee(d$keys(), c("a", "b"))
d$delete("a")
ee(d$keys(), "b")


# -------
# peek_at
# -------
d = Dict$new(a = 1, b = 3)
ee(d$peek_at(1), Dict$new(a = 1))
ee(d$peek_at(2), Dict$new(b = 3))
ee(d$peek_at(c("a", "b")), d)
ee(d$peek_at(list(1, "b")), d)
ee(d$peek_at(), d)

expect_error(d$peek_at(c(1, 1)), "duplicated keys are not allowed")

ee(d$peek_at(1:2), d)
ee(d$peek_at("a"), d$peek_at(match("a", names(d))))
ee(d$peek_at("b"), d$peek_at(match("b", names(d))))

ee(d$peek_at(0), Dict$new())
ee(d$peek_at(-1), Dict$new())
ee(d$peek_at("c"), Dict$new())
ee(d$peek_at(as.numeric(NA)), Dict$new())

expect_error(d$peek_at(0, default = "foo"), "all elements must be named")
expect_error(d$peek_at(9, default = "foo"), "all elements must be named")
expect_error(d$peek_at(NA, default = "foo"), "all elements must be named")
ee(d$peek_at(NULL, default = "foo"), Dict$new())

ee(d$peek_at("z", default = "foo"), Dict$new(z = "foo"))

ee(d$peek_at(list("a", "x", 9)), Dict$new(a = 1))
ee(d$peek_at(c("a", "x", "z"), default = 0),
   Dict$new(a = 1, x = 0, z = 0))

ee(d$peek_at(list(a = NULL), default = 0), Dict$new())

ee(d$peek_at(list("s1" = "a", "s2" = "x", "s3" = 9), default = -1),
   Dict$new(a = 1, s3 = -1, x = -1))

ee(d$peek_at(list(s1 = "a", s2 = "x", s3 = NULL, s4 = 9), default = 0),
   Dict$new(a = 1, x = 0, s4 = 0))

ee(d$peek_at(c(s1 = "a", s2 = "x"), default = -1),
   Dict$new(a = 1, x = -1))

ee(d$peek_at(c(s1 = 1, s2 = 2, s3 = 9), default = -1),
   Dict$new(s1.a = 1, s2.b = 3, s3 = -1))

ee(d$peek_at(c(s1 = "a", s2 = 2, s3 = 9), default = -1),
   Dict$new(a = 1, "2" = -1, "9" = -1))


# --------
# peek_at2
# --------
d = Dict$new(a = 1, b = 3)
ee(d$peek_at2(1), 1)
ee(d$peek_at2(2), 3)
ee(d$peek_at2("a"), 1)
ee(Dict$new()$peek_at2(1), NULL)
ee(Dict$new()$peek_at2(1, default = 0), 0)
ee(d$peek_at2(), d$peek_at2(NULL))

ee(d$peek_at2(1:2), NULL)
ee(d$peek_at2(1:2, default = 0), 0)
ee(d$peek_at2(0), NULL)
ee(d$peek_at2(0, default = "foo"), "foo")
ee(d$peek_at2(-1), NULL)
ee(d$peek_at2(-1, default = "foo"), "foo")
ee(d$peek_at2(5, default = 0), 0)
ee(d$peek_at2(as.numeric(NA), default = 0), 0)
ee(d$peek_at2(c("a", "b"), 0), 0)
ee(d$peek_at2("c", 0), 0)



# ---
# pop
# ---
# elements can be popped and popping non-existent keys gives an error
d <- Dict$new(a = 1, b = 2, c = 3)
ee(d$pop("a"), 1)
expect_false(d$has("a"))
expect_error(d$pop("a"))

ee(d$pop(), 3)
expect_error(Dict$new()$pop(), "pop at empty Dict")


# ------
# rename
# ------
d <- Dict$new(A = 1, B = 2)
expect_error(d$rename(1, "C"), "'old' must be character")
expect_error(d$rename("A", 1), "'new' must be character")
expect_error(d$rename("A", c("C", "D")),
             "'old' and 'new' names must be of the same length")
expect_error(d$rename("A", "B"), "key 'B' already in Dict")
expect_error(d$rename("Z", "B"), "Items of 'old' not found in names: Z")

vals = as.numeric(d$values())
d$rename("A", "a")
expect_true(d$has("a"))
expect_false(d$has("A"))

# Verify that values did not change
values_did_not_change = all.equal(vals, as.numeric(d$values()))
expect_true(values_did_not_change)

# Several keys at once
d$rename(c("a", "B"), c("x", "y"))
ee(d$keys(), c("x", "y"))

# Renaming same key multiple times is possible
expect_error(d$rename(c("x", "x2"), c("x2", "x3")),
             "Items of 'old' not found in names: x2")


# -------
# replace
# -------
# Signals an error if elem does not exist unless add == TRUE
d = Dict$new()
expect_error(d$replace(0, 1), "old element \\(0\\) is not in Dict")
expect_error(d$replace(NULL, 1), "old element \\(NULL\\) is not in Dict")

d = Dict$new(a = 0)
expect_error(d$replace(1, 2), "old element \\(1\\) is not in Dict")

# add == TRUE is not supported for Dict objects
expect_error(d$replace(1, 2, add = TRUE), "unused argument")


# Replacing by new element works as expected
d = Dict$new(a = 1, b = 2)
ee(d$replace(1, 2), Dict$new(a = 2, b = 2))

d = Dict$new(a = 1, b = "1")
ee(d$replace(1, 0), Dict$new(a = 0, b = "1"))

# Replace works on special elements of basic type
d = Dict$new(a = NULL, b = numeric(0), c = list())
ee(d$replace(NULL, 0), Dict$new(a = 0, b = numeric(), c = list()))
ee(d$replace(numeric(0), 0), Dict$new(a = 0, b = 0, c = list()))
ee(d$replace(list(), 0), Dict$new(a = 0, b = 0, c = 0))

# Replace works on non-basic objects
d1 = Dict$new(a = 1, b = "1")
d2 = Dict$new(a = 2, b = "2")
co = Container$new(NULL)
d = Dict$new(d1 = d1, d2 = d2, co = co)
ee(d$replace(d1, 1), Dict$new(d1 = 1, d2 = d2, co = co))
ee(d$replace(d2, 2), Dict$new(d1 = 1, d2 = 2, co = co))
ee(d$replace(co, 0), Dict$new(d1 = 1, d2 = 2, co = 0))

# ----------
# replace_at
# ----------
EE = expect_error
d = Dict$new(a = 1, b = 2)

# Requires two arguments index and value
EE(d$replace_at(1), 'argument "value" is missing, with no default')
EE(d$replace_at(value = 1), "'index' is missing")

# Signals invalid indices
EE(d$replace_at(0, 9), "index must be > 0")
EE(d$replace_at(4, 9), "index 4 exceeds length of Dict, which is 2")
EE(d$replace_at("x", 9), "index 'x' not found")
expect_error(d$replace_at(0, 0), "index must be > 0")

# If add == TRUE element is always added
d = Dict$new()
ee(d$replace_at("a", 9, add = TRUE), Dict$new(a = 9))


#ee(co$replace_at("b", 7, add = TRUE), Container$new(9, b = 7))
#ee(co$replace_at(10, NULL, add = TRUE), Container$new(9, b = 7, NULL))

# Standard cases work as expected
ee(d$replace_at("a", 0), Dict$new(a = 0, b = 2))
ee(d$replace_at(1, 9), Dict$new(a = 9, b = 2))
ee(d$replace_at("b", 0), Dict$new(a = 9, b = 0))
ee(d$replace_at(2, 9), Dict$new(a = 9, b = 9))

# Replace can replace by special elements of basic type
d = Dict$new(a = NULL, b = numeric(0))
ee(d$replace_at("a", integer()), Dict$new(a = integer(), b = numeric(0)))
ee(d$replace_at(1, list()), Dict$new(a = list(), b = numeric(0)))
ee(d$replace_at(2, NULL), Dict$new(a = list(), b = NULL))


# ------
# update
# ------
# a Dict can be updated by another Dict object
d0 <- Dict$new(A = 0)
d1 <- Dict$new(A = 1, B = 2, C = 12)
d2 <- Dict$new(              C = 3, D = 4)

ee(d0$update(d0),         Dict$new(A = 0))
ee(d0$update(Dict$new()), Dict$new(A = 0))
ee(Dict$new()$update(d0), Dict$new(A = 0))

ee(d1$update(Dict$new()), d1)
ee(d1$update(d2)$values(), list(A = 1, B = 2, C = 3, D = 4))
ee(Dict$new()$update(d2), d2)

# a Dict can be updated by another Container object if all elements are named
d <- Dict$new(A = 0)
co = Container$new(A = 1, B = 2)
ee(d$update(co), Dict$new(A = 1, B = 2))

co = Container$new(C = 1, 2)
expect_error(d$update(co), "all elements of 'other' must be named")

# -----
# clone
# -----
# Since internally, the elements of a Dict are stored in an environment,
# Dict objects always provide reference semantics when cloned non-deeply
d1 <- Dict$new(a = 1, b = 2, c = 3)
d2 <- d1
dd <- d1$clone()
dd.deep <- d1$clone(deep = TRUE)
expect_true(identical(d1, d2))
expect_false(identical(d1, dd))
ee(d1$values(), dd$values())
ee(d1$values(), dd.deep$values())

d1$delete("c")
expect_true(identical(d1, d2))
ee(d1$values(), dd$values())
ee(dd$values(), list(a = 1, b = 2))
ee(dd.deep$values(), list(a = 1, b = 2, c = 3))

# Dict containing Dict objects can be cloned deeply as well
d1 = Dict$new(a = 1)
d2 = Dict$new(d1 = d1)

dd = d2$clone()
ee(dd, Dict$new(d1 = Dict$new(a = 1)))

d1$add("b", 2)   # since it was not a deep clone, this will affect dd
ee(dd, Dict$new(d1 = Dict$new(a = 1, b = 2)))

dd.deep = d2$clone(deep = TRUE)
d1$add("c", 3)   # this again affects dd but not dd.deep
ee(dd,      Dict$new(d1 = Dict$new(a = 1, b = 2, c = 3)))
ee(dd.deep, Dict$new(d1 = Dict$new(a = 1, b = 2)))

# The deep copy also works for double-nested dict
d3 = Dict$new(d2 = d2)
ddd.deep = d3$clone(deep = TRUE)
d1$add("d", 4)
ee(d3,
   Dict$new(d2 = Dict$new(d1 = Dict$new(a = 1, b = 2, c = 3, d = 4))))
ee(ddd.deep,
   Dict$new(d2 = Dict$new(d1 = Dict$new(a = 1, b = 2, c = 3))))


