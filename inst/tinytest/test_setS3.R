# ------
# setnew
# ------
s <- setnew()
expect_true(is.set(s))
expect_equal(length(s), 0)
expect_equal(names(s), NULL)
expect_equal(attr(s, "class"), c("Set", "Container", "Iterable", "R6"))

# set elements can be named
s <- setnew(a = 2, b = 1, 9)
expect_equal(names(s), c("b", "a", ""))

# ------
# as.set
# ------
expect_equal(as.set(numeric()), setnew())
expect_equal(as.set(NULL), setnew())
expect_equal(as.set(list()), setnew())
expect_equal(as.set(1), setnew(1))
expect_equal(as.set(1:2), setnew(1L, 2L))
expect_equal(as.set(setnew()), setnew())
expect_equal(as.set(setnew(1)), setnew(1))

# set can be created as copy from another set
s = setnew(1, 2)
s2 = as.set(s)
expect_equal(s, s2)
s$clear()
expect_equal(length(s), 0)
expect_equal(length(s2), 2)

# a data.frame can be converted to a set
daf = data.frame(A = 1:2, B = 3:4)
expect_equal(as.list(as.set(daf)), as.list(daf))

# a set can be converted to a list
s = setnew(1, 2)
expect_equal(as.list(s), list(1, 2))

# a deque can be converted to a set
d = deque(1, 2)
expect_equal(as.list(as.set(d)), list(1, 2))

# a dict can be converted to a set
d = dict(a = 1, b = 2)
expect_equal(as.list(as.set(d)), list(a = 1, b = 2))

# ------
# is.set
# ------
expect_error(is.set())
expect_false(is.set(0))
expect_false(is.set(list()))
expect_false(is.set(container()))
expect_false(is.set(dict()))
expect_false(is.set(deque()))

expect_true(is.set(setnew()))
expect_true(is.set(setnew(NULL)))
expect_true(is.set(setnew()))


# -----------
# c.Container
# -----------
# standard non-recursive
expect_equal(as.list(c(setnew())), c(list()))
expect_equal(as.list(c(setnew(1))), c(list(1)))
expect_equal(as.list(c(setnew(NULL))), c(list(NULL)))

expect_equal(as.list(c(setnew(), setnew())),
                     c(list(),     list()))
expect_equal(as.list(c(setnew(1), setnew())),
                     c(list(1), list()))
expect_equal(as.list(c(setnew(1), setnew(2))),
                     c(list(1), list(2)))
expect_equal(as.list(c(setnew(1), setnew(2, list(a = 3)))),
                     c(list(1), list(2, list(a = 3)))[c(3, 1, 2)])
expect_equal(as.list(c(setnew(1), setnew(2, setnew(a = 3)))),
                     c(list(1), list(2, setnew(a = 3)))[c(3, 1, 2)])

expect_equal(names(c(setnew(1), dict(a = 2, b = setnew(a = 3)), use.names = FALSE)), NULL)


# recursive
cr = function(...) c(..., recursive = TRUE)
expect_equal(cr(setnew()),
             cr(  list()))
expect_equal(cr(setnew(1)),
             cr(  list(1)))
expect_equal(cr(setnew(NULL)),
             cr(  list(NULL)))

expect_equal(cr(setnew(), setnew()),
             cr(list(), list()))
expect_equal(cr(setnew(1), setnew()),
             cr(list(1), list()))
expect_equal(cr(setnew(1), setnew(2)),
             cr(list(1), list(2)))
expect_equal(cr(setnew(1), setnew(2, 3)),
             cr(list(1), list(2, 3)))
expect_equal(cr(setnew(1), setnew(2, list(a = 3))),
             cr(list(1), list(2, list(a = 3)))[c(1, 3, 2)])
expect_equal(cr(setnew(1), setnew(2, setnew(a = 3))),
             cr(list(1), list(2, list(a = 3)))[c(1, 3, 2)])
expect_equal(cr(setnew(1), list(2, setnew(a = 3))),
             cr(list(1), list(2, list(a = 3))))
expect_equal(cr(setnew(1), list(2, dict(a = 3))),
             cr(list(1), list(2, list(a = 3))))
expect_equal(cr(setnew(), list(2, dict(a = 3))),
             cr(list(), list(2, list(a = 3))))

expect_equal(c(setnew(1), dict(a = 2, b = setnew(a = 3)), recursive = TRUE),
             c(1, a = 2, b.a = 3))


# Ensure concatenated objects are always copies
c1 = setnew(1)
c2 = setnew(2)
c1c1 = setnew(c1 = c1)

cc = c(c1, c1c1, c2)
expect_equal(unpack(cc), c(c1 = 1, 1, 2))
c1$add(2)
expect_equal(unpack(cc), c(c1 = 1, 1, 2)) # still the same


