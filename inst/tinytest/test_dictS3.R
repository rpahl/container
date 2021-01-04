# dict initialization works as expected
d = dict()
expect_true(inherits(d, "Dict"))
expect_true(inherits(d, "Container"))
expect_true(empty(dict()))
expect_equal(mode(values(dict())), "list")
expect_error(dict(1:2), "all elems must be named")
expect_error(dict(x = 1, y = 2, x = 3), "duplicated keys")
expect_equal(keys(dict()), character(0))

# One element
expect_equal(values(dict(x = 1)), list(x = 1))
expect_equal(values(dict(x = NULL)), list(x = NULL))
expect_equal(values(dict(x = 1:4)), list(x = 1:4))
env = new.env()
expect_equal(values(dict(x = env)), list(x = env))

# Two (or more) elements
d <- dict(x = 1, y = 2)
expect_equal(values(d), values(container(x = 1, y = 2, keep_names = TRUE)))
expect_equal(names(values(d)), c("x", "y"))

expect_equal(length(dict(env = environment(), id = identity)), 2)


# adding elements to a Dict required a character key and a value
d <- dict()
expect_error(add(d, 1), "key must be character")
expect_error(add(d, "", 1, "zero-length key"))
expect_error(add(d, "a"), 'argument "value" is missing')
expect_error(add(d, value = 1), 'argument "key" is missing')
expect_error(add(d, c("a", "b"), 1:2), 'key must be of length 1')
expect_error(add(d, 1, 1), 'key must be character')


# added elements must have distinct keys and cannot be added twice
d <- dict()
add(d, "a", 1)
expect_equal(values(d), list(a = 1))
expect_error(add(d, "a", 1), "key 'a' already in Dict")
expect_error(add(d, "a", 2), "key 'a' already in Dict")
expect_error(add(d, "a", NULL), "key 'a' already in Dict")


# NULL and empty lists can be added
d <- dict()
add(d, "null", NULL)
add(d, "empty-list", list())
expect_equal(values(d), list("null" = NULL, "empty-list" = list()))


# elements can be deleted from a Dict
d <- dict(a = 1)
expect_false(empty(d))
expect_true(empty(delete(d, "a")))


# if key not in Dict, trying to delete it gives an error
expect_error(delete(dict(a = 1), "b"), "key 'b' not in Dict")


# only one key can be deleted at a time
d <- dict(a = 1, b = 2)
expect_error(delete(d, c("a", "b"), "key must be of length 1"),
             "key must be of length 1")

# failed delete does not alter the dict object
d_was_not_touched <- length(d) == 2
expect_true(d_was_not_touched)


# elements can be discarded
d <- dict(a = 1)
expect_false(empty(d))
expect_true(empty(discard(d, "a")))


# discard ignores non-existing elements without error
d <- dict(a = 1)
expect_equal(values(d), list(a = 1))
discard(d, "b")
expect_equal(values(d), list(a = 1))


# only one key can be discarded at a time
d <- dict(a = 1, b = 2)
expect_error(discard(d, c("a", "b"), "key must be of length 1"),
             "key must be of length 1")
d_was_not_touched <- length(d) == 2
expect_true(d_was_not_touched)


# getting an element throws an error if key does not exist
d <- dict(a = 1, b = 2, n = NULL)
expect_equal(getval(d, "a"), 1)
expect_true(is.null(getval(d, "n")))
expect_error(getval(d, "x"), "key 'x' not in Dict")


# only one element at a time can be 'get'
d <- dict(a = 1, b = 2)
expect_error(getval(d, c("a", "b")), "key must be of length 1")


# it can be checked if Dict has a certain key
d <- dict(a = 1, b = 2)
expect_true(has(d, "a"))
expect_false(has(d, "x"))
expect_error(has(d, c("a", "b")), "key must be of length 1")


# all keys can be listed
d <- dict(a = 1, b = 2)
expect_equal(keys(d), c("a", "b"))
delete(d, "a")
expect_equal(keys(d), "b")


# elements can be peeked and return default value if key does not exist
d <- dict(a = 1, b = 2)
expect_equal(peek(d, "a"), getval(d, "a"))
expect_true(is.null(peek(d, "x")))
expect_equal(peek(d, "x", default = 9), 9)


# elements can be popped and popping non-existent element gives an error
d <- dict(a = 1, b = 2)
expect_equal(pop(d, "a"), 1)
expect_false(has(d, "a"))
expect_error(pop(d, "a"))


# elements can be popped randomly from Dict
x <- c(a = 1, b = 2)
d <- as.dict(x)
v <- numeric(0)
while (!empty(d)) {
    v <- c(v, popitem(d))
}
expect_equal(sort(v), as.numeric(x))
expect_true(empty(d))
expect_error(popitem(d), "popitem at empty Dict")


# A key in the Dict can be renamed
d <- dict(A = 1, B = 2)
expect_error(rename(d, 1, "C"), "'old' must be character")
expect_error(rename(d, "A", 1), "'new' must be character")
expect_error(rename(d, "A", c("C", "D")), "must be of the same length")
expect_error(rename(d, "A", "B"), "rename failed because 'B' exists already")
expect_error(rename(d, "Z", "B"), "key 'Z' not found")

vals = as.numeric(values(d))
rename(d, "A", "a")
expect_true(has(d, "a"))
expect_false(has(d, "A"))

# Verify that values did not change
expect_equal(vals, as.numeric(values(d)))

# Several keys at once
rename(d, c("a", "B"), c("x", "y"))
expect_equal(keys(d), c("x", "y"))

# Renaming same key multiple times is possible
rename(d, c("x", "x2"), c("x2", "x3"))
expect_equal(keys(d), c("x3", "y"))


# an existing value can be changed in a Dict
d <- dict(a = 1, b = NULL)
setval(d, "b", list(1, 2))
expect_equal(getval(d, "b"), list(1, 2))
expect_error(setval(d, "x", 1), "key 'x' not in Dict")


# a Dict can be re-sorted according to its keys
d <- dict()
add(d, "b", 1)
add(d, "a", 2)
expect_equal(keys(d), c("b", "a"))
expect_equal(keys(sortkey(d)), c("a", "b"))


# a Dict can be updated by another Dict object
d1 <- dict(A = 1, B = 2, C = 12)
d2 <- dict(              C = 3, D = 4)
expect_error(update(d1, list()), "'other' must be a Dict")
expect_equal(update(d1, dict()), d1)
expect_equal(values(update(d1, d2)), list(A = 1, B = 2, C = 3, D = 4))
expect_equal(update(dict(), d2), d2)


# Conversion

# data.frame can be converted to dict
df <- data.frame(A = 1:5, B = 1:5)
d <- as.dict(df)
expect_equal(as.data.frame(as.list(d)), df)

