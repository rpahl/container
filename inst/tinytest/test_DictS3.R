# ----
# dict
# ----
d = dict()
expect_true(is.dict(d))
expect_equal(length(d), 0)
expect_equal(names(d), NULL)
expect_equal(attr(d, "class"), c("Dict", "Container", "Iterable", "R6"))

expect_true(is_empty(dict()))
expect_equal(mode(as.list(dict())), "list")
expect_error(dict(1:2), "all elements must be named")
expect_error(dict(x = 1, y = 2, x = 3), "duplicated keys")

# dict of dict is also a copy throughout
d1 = dict(a = 1)
d2 = dict(d = d1)
d1$clear()
expect_equal(d2, dict(d = dict(a = 1)))

# -------
# as.dict
# -------
expect_equal(as.dict(numeric()), dict())
expect_equal(as.dict(NULL), dict())
expect_equal(as.dict(list()), dict())
expect_equal(as.dict(c(a = 1)), dict(a = 1))
expect_equal(as.dict(list(a = 1, b = 2)), dict(a = 1, b = 2))
expect_equal(as.dict(dict(a = 1)), dict(a = 1))

# a data.frame can be converted to a dict
daf = data.frame(A = 1:2, B = 3:4)
expect_equal(as.list(as.dict(daf)), as.list(daf))

# a dict can be converted to a list
d = dict(a = 1, b = 2)
expect_equal(as.list(d), list(a = 1, b = 2))

# a set can be converted to a dict
s = setnew(a = 1, b = 2)
expect_equal(as.dict(s), dict(a = 1, b = 2))

# -------
# is.dict
# -------
expect_error(is.dict())
expect_false(is.dict(0))
expect_false(is.dict(list()))
expect_false(is.dict(container()))
expect_false(is.dict(setnew()))
expect_false(is.dict(deque()))

expect_true(is.dict(dict()))
expect_true(is.dict(dict(a = NULL)))
expect_true(is.dict(dict()))



# ------
# c.Dict
# ------

# standard non-recursive
expect_equal(as.list(c(dict())), c(list()))
expect_equal(as.list(c(dict(x = 1))), c(list(x = 1)))
expect_equal(as.list(c(dict(x = NULL))), c(list(x = NULL)))

expect_equal(as.list(c(dict(), dict())),
                     c(list(), list()))
expect_equal(as.list(c(dict(a = 1), dict())),
                     c(list(a = 1), list()))
expect_equal(as.list(c(dict(a = 1), dict(b = 2))),
                     c(list(a = 1), list(b = 2)))
expect_equal(as.list(c(dict(a = 1), dict(b = 2, l = list(a = 3)))),
                     c(list(a = 1), list(b = 2, l = list(a = 3))))
expect_equal(as.list(c(dict(a = 1), d = dict(b = 2, d = dict(a = 3)))),
                     c(list(a = 1), d = list(b = 2, d = dict(a = 3))))
expect_equal(c(dict(a = 1), dict(x = 2, b = dict(a = 3))),
               dict(a = 1,       x = 2, b = dict(a = 3)))



# recursive
cr = function(...) c(..., recursive = TRUE)
expect_equal(cr(dict()),
             cr(list()))
expect_equal(cr(dict(a = 1)),
             cr(list(a = 1)))
expect_equal(cr(dict(x = NULL)),
             cr(list(x = NULL)))

expect_equal(cr(dict(), dict()),
             cr( list(),  list()))
expect_equal(cr(dict(a = 1), dict()),
             cr(list(a = 1), list()))
expect_equal(cr(dict(a = 1), dict(b = 2)),
             cr(list(a = 1), list(b = 2)))
expect_equal(cr(dict(a = 1), dict(b = 2, c = 3)),
             cr(list(a = 1), list(b = 2, c = 3)))
expect_equal(cr(dict(a = 1), dict(b = 2, l = list(a = 3))),
             cr(list(a = 1), list(b = 2, l = list(a = 3))))
expect_equal(cr(dict(a = 1), dict(b = 2, d = dict(a = 3))),
             cr(list(a = 1), list(b = 2, d = list(a = 3))))
expect_equal(cr(dict(a = 1), list(b = 2, d = dict(a = 3))),
             cr(list(a = 1), list(b = 2, d = list(a = 3))))
expect_equal(cr(dict(a = 1), l = list(b = 2, d = dict(a = 3))),
             cr(list(a = 1), l = list(b = 2, d = list(a = 3))))
expect_equal(cr(dict(), l = list(b = 2, x = dict(a = 3))),
             cr(list(), l = list(b = 2, x = list(a = 3))))

expect_equal(c(dict(a = 1), dict(a = 2, b = dict(a = 3)), recursive = TRUE),
             c(a = 1, a = 2, b.a = 3))


# Ensure concatenated objects are always copies
c1 = dict(a = 1)
c2 = dict(b = 2)
c1c1 = dict(c1 = c1)

cc = c(c1, c1c1, c2)
expect_equal(unpack(cc), c(a = 1, b = 2, c1.a = 1))
c1$add("y", 2)
still_the_same = expect_equal(unpack(cc), c(a = 1, b = 2, c1.a = 1))
expect_true(still_the_same)



# ----------
# S3 methods
# ----------
exit_file(msg = "Dict S3 methods - todo")

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
expect_equal(as.list(d), list(a = 1))
expect_error(add(d, "a", 1), "key 'a' already in Dict")
expect_error(add(d, "a", 2), "key 'a' already in Dict")
expect_error(add(d, "a", NULL), "key 'a' already in Dict")


# NULL and empty lists can be added
d <- dict()
add(d, "empty-list", list())
add(d, "null", NULL)
expect_equal(as.list(d), list("empty-list" = list(), "null" = NULL))


# elements can be deleted from a Dict
d <- dict(a = 1)
expect_false(is_empty(d))
expect_true(is_empty(delete(d, "a")))

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
expect_false(is_empty(d))
expect_true(is_empty(discard(d, "a")))


# discard ignores non-existing elements without error
d <- dict(a = 1)
expect_equal(as.list(d), list(a = 1))
discard(d, "b")
expect_equal(as.list(d), list(a = 1))


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
while (!is_empty(d)) {
    v <- c(v, popitem(d))
}
expect_equal(sort(v), as.numeric(x))
expect_true(is_empty(d))
expect_error(popitem(d), "popitem at empty Dict")


# an existing value can be changed in a Dict
d <- dict(a = 1, b = NULL)
setval(d, "b", list(1, 2))
expect_equal(getval(d, "b"), list(1, 2))
expect_error(setval(d, "x", 1), "key 'x' not in Dict")


# Conversion
expect_equal(as.dict(NULL), dict())
expect_equal(as.dict(c(a = 1)), dict(a = 1))

# data.frame can be converted to dict
df <- data.frame(A = 1:5, B = 1:5)
d <- as.dict(df)
expect_equal(as.data.frame(as.list(d)), df)

