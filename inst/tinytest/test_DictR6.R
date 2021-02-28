# ----------
# initialize
# ----------
# Dict constructor works as expected
d = Dict$new()

expect_equal(attr(d, "class"), c("Dict", "Container", "Iterable", "R6"))
expect_equal(mode(d$values()), "list")

d <- Dict$new(env = environment())
expect_equal(d$length(), 1)

d <- Dict$new(env = environment(), foo = identity)
expect_equal(d$length(), 2)

expect_true(Dict$new()$empty())

expect_error(Dict$new(1:2), "all elems must be named")
expect_error(Dict$new(x = 1, y = 2, x = 3), "duplicated keys")
expect_equal(Dict$new()$keys(), character(0))

# One element
expect_equal(Dict$new(x = 1)$values(), list(x = 1))
expect_equal(Dict$new(x = NULL)$values(), list(x = NULL))
expect_equal(Dict$new(x = 1:4)$values(), list(x = 1:4))
env = new.env()
expect_equal(Dict$new(x = env)$values(), list(x = env))

# Two (or more) elements
d <- Dict$new(x = 1:2, y = 2:3)
expect_equal(d$values(),
             Container$new(x = 1:2, y = 2:3)$values())
expect_equal(names(d$values()), c("x", "y"))

# a Dict's keys are always sorted
v <- c(h = 1, d = 2, a = 8, b = 0)
d <- as.dict(v)
expect_equal(keys(d), sort(names(v)))


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
expect_equal(d$values(), list(a = 1))
expect_error(d$add("a", 1), "key 'a' already in Dict")
expect_error(d$add("a", 2), "key 'a' already in Dict")
expect_error(d$add("a", NULL), "key 'a' already in Dict")


# NULL and empty lists can be added
d <- Dict$new()
d$add("empty-list", list())
d$add("null", NULL)
expect_equal(d$values(), list("empty-list" = list(), "null" = NULL))


# ------
# delete
# ------
# elements can be deleted from a Dict
d <- Dict$new(a = 1)
expect_false(d$empty())
expect_true(d$delete("a")$empty())

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
expect_false(d$empty())
expect_true(d$discard("a")$empty())


# -------
# discard
# -------
# discard ignores non-existing elements without error
d <- Dict$new(a = 1)
expect_equal(d$values(), list(a = 1))
d$discard("b")
expect_equal(d$values(), list(a = 1))


# only one key can be discarded at a time
d <- Dict$new(a = 1, b = 2)
expect_error(d$discard(c("a", "b"), "key must be of length 1"),
             "key must be of length 1")
d_was_not_touched <- d$length() == 2
expect_true(d_was_not_touched)


# -------
# discard
# -------
# trying to extract from non-existing key throws an error
d <- Dict$new(a = 1, b = 2, n = NULL)
expect_equal(d$getvalue("a"), 1)
expect_true(is.null(d$getvalue("n")))
expect_error(d$getvalue("x"), "key 'x' not in Dict")


# only one key at a time can be accessed
d <- Dict$new(a = 1, b = 2)
expect_error(d$getvalue(c("a", "b")), "key must be of length 1")


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
expect_equal(d$keys(), c("a", "b"))
d$delete("a")
expect_equal(d$keys(), "b")


# ----
# peek
# ----
# elements can be peeked and return default value if key does not exist
d <- Dict$new(a = 1, b = 2)
expect_equal(d$peek("a"), d$getvalue("a"))
expect_true(is.null(d$peek("x")))
expect_equal(d$peek("x", default = 9), 9)

# --------
# peekitem
# --------
d <- Dict$new(a = 1, b = 2)
expect_equal(length(d$peekitem()), 1)
expect_true(d$peekitem() %in% 1:2)
expect_equal(d$length(), 2)

# ---
# pop
# ---
# elements can be popped and popping non-existent keys gives an error
d <- Dict$new(a = 1, b = 2)
expect_equal(d$pop("a"), 1)
expect_false(d$has("a"))
expect_error(d$pop("a"))

# -------
# popitem
# -------
# elements can be popped randomly
x <- c(a = 1, b = 2)
d <- as.dict(x)
v <- numeric(0)
while (!d$empty()) {
    v <- c(v, d$popitem())
}
expect_equal(sort(v), as.numeric(x))
expect_true(d$empty())
expect_error(d$popitem(), "popitem at empty Dict")


# ------
# rename
# ------
d <- Dict$new(A = 1, B = 2)
expect_error(d$rename(1, "C"), "key must be character")
expect_error(d$rename("A", 1), "key must be character")
expect_error(d$rename("A", c("C", "D")), "must be of same length")
expect_error(d$rename("A", "B"), "rename failed because 'B' exists already")
expect_error(d$rename("Z", "B"), "key 'Z' not found")

vals = as.numeric(d$values())
d$rename("A", "a")
expect_true(d$has("a"))
expect_false(d$has("A"))

# Verify that values did not change
values_did_not_change = all.equal(vals, as.numeric(d$values()))
expect_true(values_did_not_change)

# Several keys at once
d$rename(c("a", "B"), c("x", "y"))
expect_equal(d$keys(), c("x", "y"))

# Renaming same key multiple times is possible
d$rename(c("x", "x2"), c("x2", "x3"))
expect_equal(d$keys(), c("x3", "y"))


# -------
# replace
# -------
# values at keys can be replaced
d <- Dict$new(a = 1, b = NULL)
d$replace("b", list(1, 2))
expect_equal(d$getvalue("b"), list(1, 2))
expect_error(d$replace("x", 1), "key 'x' not in Dict")


# ------
# update
# ------
# a Dict can be updated by another Dict object
d0 <- Dict$new(A = 0)
d1 <- Dict$new(A = 1, B = 2, C = 12)
d2 <- Dict$new(              C = 3, D = 4)

expect_equal(d0$update(d0),         Dict$new(A = 0))
expect_equal(d0$update(Dict$new()), Dict$new(A = 0))
expect_equal(Dict$new()$update(d0), Dict$new(A = 0))

expect_error(d1$update(list()), "arg must be a Dict")
expect_equal(d1$update(Dict$new()), d1)
expect_equal(d1$update(d2)$values(), list(A = 1, B = 2, C = 3, D = 4))
expect_equal(Dict$new()$update(d2), d2)

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
expect_equal(d1$values(), dd$values())
expect_equal(d1$values(), dd.deep$values())

d1$delete("c")
expect_true(identical(d1, d2))
expect_equal(d1$values(), dd$values())
expect_equal(dd$values(), list(a = 1, b = 2))
expect_equal(dd.deep$values(), list(a = 1, b = 2, c = 3))

# Dict containing Dict objects can be cloned deeply as well
d1 = Dict$new(a = 1)
d2 = Dict$new(d1 = d1)

dd = d2$clone()
expect_equal(dd, Dict$new(d1 = Dict$new(a = 1)))

d1$add("b", 2)   # since it was not a deep clone, this will affect dd
expect_equal(dd, Dict$new(d1 = Dict$new(a = 1, b = 2)))

dd.deep = d2$clone(deep = TRUE)
d1$add("c", 3)   # this again affects dd but not dd.deep
expect_equal(dd,      Dict$new(d1 = Dict$new(a = 1, b = 2, c = 3)))
expect_equal(dd.deep, Dict$new(d1 = Dict$new(a = 1, b = 2)))

# The deep copy also works for double-nested dict
d3 = Dict$new(d2 = d2)
ddd.deep = d3$clone(deep = TRUE)
d1$add("d", 4)
expect_equal(d3,
             Dict$new(d2 = Dict$new(d1 = Dict$new(a = 1, b = 2, c = 3, d = 4))))
expect_equal(ddd.deep,
             Dict$new(d2 = Dict$new(d1 = Dict$new(a = 1, b = 2, c = 3))))


# ----------
# deprecated
# ----------
# verify that functions are deprecated

# Dict set is deprecated and replaced by replace
d <- Dict$new(a = 1, b = 2)
expect_warning(d$set("b", 9), "Use 'replace' instead.")
expect_equal(d$peek("b"), 9)

# Dict remove is deprecated and replaced by delete
d <- Dict$new(a = 1, b = 2)
expect_warning(d$remove("b"), "Use 'delete' instead.")
expect_false(d$has("b"))

# Dict sort is deprecated
d <- Dict$new(b = 1, a = 2)
expect_warning(d$sort(), "'sort' is deprecated - keys are now always sorted")

