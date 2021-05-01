ee = expect_equal

# ----
# dict
# ----
d = dict()
expect_true(is.dict(d))
ee(length(d), 0)
ee(names(d), NULL)
ee(attr(d, "class"), c("Dict", "Container", "Iterable", "R6"))

expect_true(is_empty(dict()))
ee(mode(as.list(dict())), "list")
expect_error(dict(1:2), "all elements must be named")
expect_error(dict(x = 1, y = 2, x = 3), "duplicated keys")

# dict of dict is also a copy throughout
d1 = dict(a = 1)
d2 = dict(d = d1)
d1$clear()
ee(d2, dict(d = dict(a = 1)))

# -------
# as.dict
# -------
ee(as.dict(numeric()), dict())
ee(as.dict(NULL), dict())
ee(as.dict(list()), dict())
ee(as.dict(c(a = 1)), dict(a = 1))
ee(as.dict(list(a = 1, b = 2)), dict(a = 1, b = 2))
ee(as.dict(dict(a = 1)), dict(a = 1))

# a data.frame can be converted to a dict
daf = data.frame(A = 1:2, B = 3:4)
ee(as.list(as.dict(daf)), as.list(daf))

# a dict can be converted to a list
d = dict(a = 1, b = 2)
ee(as.list(d), list(a = 1, b = 2))

# a set can be converted to a dict
s = setnew(a = 1, b = 2)
ee(as.dict(s), dict(a = 1, b = 2))

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
ee(as.list(c(dict())), c(list()))
ee(as.list(c(dict(x = 1))), c(list(x = 1)))
ee(as.list(c(dict(x = NULL))), c(list(x = NULL)))

ee(as.list(c(dict(), dict())),
           c(list(), list()))
ee(as.list(c(dict(a = 1), dict())),
           c(list(a = 1), list()))
ee(as.list(c(dict(a = 1), dict(b = 2))),
           c(list(a = 1), list(b = 2)))
ee(as.list(c(dict(a = 1), dict(b = 2, l = list(a = 3)))),
           c(list(a = 1), list(b = 2, l = list(a = 3))))
ee(as.list(c(dict(a = 1), d = dict(b = 2, d = dict(a = 3)))),
           c(list(a = 1), d = list(b = 2, d = dict(a = 3))))
ee(c(dict(a = 1), dict(x = 2, b = dict(a = 3))),
     dict(a = 1,       x = 2, b = dict(a = 3)))



# recursive
cr = function(...) c(..., recursive = TRUE)
ee(cr(dict()),
   cr(list()))
ee(cr(dict(a = 1)),
   cr(list(a = 1)))
ee(cr(dict(x = NULL)),
   cr(list(x = NULL)))

ee(cr(dict(), dict()),
   cr( list(),  list()))
ee(cr(dict(a = 1), dict()),
   cr(list(a = 1), list()))
ee(cr(dict(a = 1), dict(b = 2)),
   cr(list(a = 1), list(b = 2)))
ee(cr(dict(a = 1), dict(b = 2, c = 3)),
   cr(list(a = 1), list(b = 2, c = 3)))
ee(cr(dict(a = 1), dict(b = 2, l = list(a = 3))),
   cr(list(a = 1), list(b = 2, l = list(a = 3))))
ee(cr(dict(a = 1), dict(b = 2, d = dict(a = 3))),
   cr(list(a = 1), list(b = 2, d = list(a = 3))))
ee(cr(dict(a = 1), list(b = 2, d = dict(a = 3))),
   cr(list(a = 1), list(b = 2, d = list(a = 3))))
ee(cr(dict(a = 1), l = list(b = 2, d = dict(a = 3))),
   cr(list(a = 1), l = list(b = 2, d = list(a = 3))))
ee(cr(dict(), l = list(b = 2, x = dict(a = 3))),
   cr(list(), l = list(b = 2, x = list(a = 3))))

ee(c(dict(a = 1), dict(a = 2, b = dict(a = 3)), recursive = TRUE),
   c(a = 1, a = 2, b.a = 3))


# Ensure concatenated objects are always copies
c1 = dict(a = 1)
c2 = dict(b = 2)
c1c1 = dict(c1 = c1)

cc = c(c1, c1c1, c2)
ee(unpack(cc), c(a = 1, b = 2, c1.a = 1))
c1$add("y", 2)
still_the_same = ee(unpack(cc), c(a = 1, b = 2, c1.a = 1))
expect_true(still_the_same)

