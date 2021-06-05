ee = expect_equal

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
expect_error(d$add(1), "key must be character")
expect_error(d$add("", 1, "zero-length key"))
expect_error(d$add("a"), 'argument "value" is missing')
expect_error(d$add(value = 1), 'argument "key" is missing')
expect_error(d$add(c("a", "b"), 1:2), 'key must be of length 1')
expect_error(d$add(1, 1), 'key must be character')


# added elements must have distinct keys and cannot be added twice
d <- Dict$new()
d$add("a", 1)
ee(d$values(), list(a = 1))
expect_error(d$add("a", 1), "key 'a' already in Dict")
expect_error(d$add("a", 2), "key 'a' already in Dict")
expect_error(d$add("a", NULL), "key 'a' already in Dict")


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
d <- Dict$new(a = 1)
expect_false(d$is_empty())
expect_true(d$delete("a")$is_empty())

# if key not in Dict, trying to delete it gives an error
expect_error(Dict$new(a = 1)$delete("b"), "key 'b' not in Dict")

# only one key can be deleted at a time
d <- Dict$new(a = 1, b = 2)
expect_error(d$delete(c("a", "b"), "key must be of length 1"),
             "key must be of length 1")

# failed delete does not alter the dict object
d_was_not_touched <- d$length() == 2
expect_true(d_was_not_touched)

# elements can be discarded
d <- Dict$new(a = 1)
expect_false(d$is_empty())
expect_true(d$discard("a")$is_empty())


# -------
# discard
# -------
# discard ignores non-existing elements without error
d <- Dict$new(a = 1)
ee(d$values(), list(a = 1))
d$discard("b")
ee(d$values(), list(a = 1))


# only one key can be discarded at a time
d <- Dict$new(a = 1, b = 2)
expect_error(d$discard(c("a", "b"), "key must be of length 1"),
             "key must be of length 1")
d_was_not_touched <- d$length() == 2
expect_true(d_was_not_touched)


# ---
# has
# ---
# it can be checked if Dict has a certain key
d <- Dict$new(a = 1, b = 2)
expect_true(d$has("a"))
expect_false(d$has("x"))
expect_error(d$has(c("a", "b")), "key must be of length 1")

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
# values at keys can be replaced
d <- Dict$new(a = 1, b = NULL)
d$replace("b", list(1, 2))
ee(d$at2("b"), list(1, 2))
expect_error(d$replace("x", 1), "key 'x' not in Dict")


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

expect_error(d1$update(list()), "arg must be a Dict")
ee(d1$update(Dict$new()), d1)
ee(d1$update(d2)$values(), list(A = 1, B = 2, C = 3, D = 4))
ee(Dict$new()$update(d2), d2)

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


