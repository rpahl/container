ee = expect_equal

# ------
# setnew
# ------
s <- setnew()
expect_true(is.set(s))
ee(length(s), 0)
ee(names(s), NULL)
ee(attr(s, "class"), c("Set", "Container", "Iterable", "R6"))

# set elements can be named
s <- setnew(a = 2, b = 1, 9)
ee(names(s), c("a", "b", ""))

# Set of set is also a copy throughout
s1 = setnew(1)
s2 = setnew(s1)
s1$clear()
ee(s2, setnew(setnew(1)))

# ------
# as.set
# ------
ee(as.set(numeric()), setnew())
ee(as.set(NULL), setnew())
ee(as.set(list()), setnew())
ee(as.set(1), setnew(1))
ee(as.set(1:2), setnew(1L, 2L))
ee(as.set(setnew()), setnew())
ee(as.set(setnew(1)), setnew(1))

# set can be created as copy from another set
s = setnew(1, 2)
s2 = as.set(s)
ee(s, s2)
s$clear()
ee(length(s), 0)
ee(length(s2), 2)

# a data.frame can be converted to a set
daf = data.frame(A = 1:2, B = 3:4)
ee(as.list(as.set(daf)), as.list(daf))

# a set can be converted to a list
s = setnew(1, 2)
ee(as.list(s), list(1, 2))

# a deque can be converted to a set
d = deque(1, 2)
ee(as.list(as.set(d)), list(1, 2))

# a dict can be converted to a set
d = dict(a = 1, b = 2)
ee(as.list(as.set(d)), list(a = 1, b = 2))

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


# -----
# c.Set
# -----
# standard non-recursive
ee(as.list(c(setnew())), c(list()))
ee(as.list(c(setnew(1))), c(list(1)))
ee(as.list(c(setnew(NULL))), c(list(NULL)))

ee(as.list(c(setnew(), setnew())),
           c(list(),     list()))
ee(as.list(c(setnew(1), setnew())),
           c(list(1), list()))
ee(as.list(c(setnew(1), setnew(2))),
           c(list(1), list(2)))
ee(as.list(c(setnew(1), setnew(2, list(a = 3)))),
           c(list(1), list(2, list(a = 3))))

expect_true(setequal(as.list(c(setnew(1), setnew(2, setnew(a = 3)))),
                     c(list(setnew(a = 3)), list(1), list(2))))

ee(names(c(setnew(1), dict(a = 2, b = setnew(a = 3)), use.names = FALSE)), NULL)


# recursive
cr = function(...) c(..., recursive = TRUE)
ee(cr(setnew()),
   cr(  list()))
ee(cr(setnew(1)),
   cr(  list(1)))
ee(cr(setnew(NULL)),
   cr(  list(NULL)))

ee(cr(setnew(), setnew()),
   cr(list(),     list()))
ee(cr(setnew(1), setnew()),
   cr(list(1),     list()))
ee(cr(setnew(1), setnew(2)),
   cr(list(1),     list(2)))
ee(cr(setnew(1), setnew(2, 3)),
   cr(list(1),     list(2, 3)))
ee(cr(setnew(1), setnew(2, list(a = 3))),
   cr(  list(1),   list(2, list(a = 3))))
expect_true(setequal(cr(setnew(1), setnew(2, setnew(a = 3))),
                     cr(list(1),     list(list(a = 3), 2))))
ee(cr(setnew(1), list(2, setnew(a = 3))),
   cr(list(1),   list(2, list(a = 3))))
ee(cr(setnew(1), list(2, dict(a = 3))),
   cr(list(1),   list(2, list(a = 3))))
ee(cr(setnew(), list(2, dict(a = 3))),
   cr(list(),   list(2, list(a = 3))))

ee(c(setnew(1), dict(a = 2, b = setnew(a = 3)), recursive = TRUE),
   c(1, a = 2, b.a = 3))


# Ensure concatenated objects are always copies
c1 = setnew(1)
c2 = setnew(2)
c1c1 = setnew(c1 = c1)

cc = c(c1, c1c1, c2)
expect_true(setequal(unpack(cc), c(c1 = 1, 1, 2)))
c1$add(2)
expect_true(setequal(unpack(cc), c(c1 = 1, 1, 2))) # still the same


# ----------
# OrderedSet
# ----------
s = setnew(2, 1, .ordered = TRUE)
expect_true(is.orderedset(s))

ee(as.list(s), list(1, 2))
ee(as.list(as.orderedset(list(2, 1))), list(1, 2))

ee(c(s, s), s)

