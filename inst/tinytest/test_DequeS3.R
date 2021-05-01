ee = expect_equal

# -----
# deque
# -----
d <- deque()
expect_true(is.deque(d))
ee(length(d), 0)
ee(names(d), NULL)
ee(attr(d, "class"), c("Deque", "Container", "Iterable", "R6"))

# deque elements can be named
d <- deque(a = 2, 9, b = 1)
ee(names(d), c("a", "", "b"))

# deque of deque is also a copy throughout
d1 = deque(1)
d2 = deque(d1)
d1$clear()
ee(d2, deque(deque(1)))

# --------
# as.deque
# --------
ee(as.deque(numeric()), deque())
ee(as.deque(NULL), deque())
ee(as.deque(list()), deque())
ee(as.deque(1), deque(1))
ee(as.deque(1:2), deque(1L, 2L))
ee(as.deque(deque(1)), deque(1))

# deque can be created as copy from another deque
d = deque(1, 2)
d2 = as.deque(d)
ee(d, d2)
d$clear()
# if d2 is a copy, it should not have been cleared
ee(length(d), 0)
d2_was_also_cleaned = length(d2) == 0
expect_false(d2_was_also_cleaned )

# a data.frame can be converted to a deque
daf = data.frame(A = 1:2, B = 3:4)
ee(as.list(as.deque(daf)), as.list(daf))

# a deque can be converted to a list
d = deque(1, b = 2)
ee(as.list(as.deque(d)), list(1, b = 2))

# a set can be converted to a deque
s = setnew(1, 2)
ee(as.list(as.deque(s)), list(1, 2))

# a dict can be converted to a deque
d = dict(a = 1, b = 2)
ee(as.list(as.deque(d)), list(a = 1, b = 2))

# --------
# is.deque
# --------
expect_error(is.deque())
expect_false(is.deque(0))
expect_false(is.deque(list()))
expect_false(is.deque(container()))
expect_false(is.deque(dict()))
expect_false(is.deque(setnew()))

expect_true(is.deque(deque()))
expect_true(is.deque(deque(NULL)))
expect_true(is.deque(deque()))



# -----------
# c.Deque
# -----------

# standard non-recursive
ee(as.list(c(deque())), c(list()))
ee(as.list(c(deque(1))), c(list(1)))
ee(as.list(c(deque(NULL))), c(list(NULL)))

ee(as.list(c(deque(), deque())),
           c(list(), list()))
ee(as.list(c(deque(1), deque())),
           c(list(1), list()))
ee(as.list(c(deque(1), deque(2))),
           c(list(1), list(2)))
ee(as.list(c(deque(1), deque(2, list(a = 3)))),
           c(list(1), list(2, list(a = 3))))
ee(as.list(c(deque(1), deque(2, deque(a = 3)))),
           c(list(1), list(2, deque(a = 3))))
ee(c(deque(1), dict(a = 2, b = deque(a = 3))),
     deque(1,       a = 2, b = deque(a = 3)))

ee(c(deque(1), dict(a = 2, b = deque(a = 3)), use.names = FALSE),
     deque(1,           2,     deque(a = 3)))
ee(as.list(c(a = deque(1), b = deque(2, list(a = 3)), use.names = FALSE)),
           c(     list(1),      list(2, list(a = 3)), use.names = FALSE))


# recursive
cr = function(...) c(..., recursive = TRUE)
ee(cr(deque()),
   cr(list()))
ee(cr(deque(1)),
   cr(list(1)))
ee(cr(deque(NULL)),
   cr(list(NULL)))

ee(cr(deque(), deque()),
   cr( list(),  list()))
ee(cr(deque(1), deque()),
   cr( list(1),  list()))
ee(cr(deque(1), deque(2)),
   cr( list(1),  list(2)))
ee(cr(deque(1), deque(2, 3)),
   cr( list(1),  list(2, 3)))
ee(cr(deque(1), deque(2, list(a = 3))),
   cr( list(1),  list(2, list(a = 3))))
ee(cr(deque(1), deque(2, deque(a = 3))),
   cr( list(1),  list(2, list(a = 3))))
ee(cr(deque(1),  list(2, deque(a = 3))),
   cr( list(1),  list(2, list(a = 3))))
ee(cr(deque(1),  list(2, dict(a = 3))),
   cr( list(1),  list(2, list(a = 3))))
ee(cr(deque(),   list(2, dict(a = 3))),
   cr( list(),   list(2, list(a = 3))))

ee(c(deque(1), dict(a = 2, b = deque(a = 3)), recursive = TRUE),
   c(1, a = 2, b.a = 3))


# Ensure concatenated objects are always copies
c1 = deque(1)
c2 = deque(2)
c1c1 = deque(c1 = c1)

cc = c(c1, c1c1, c2)
ee(unpack(cc), c(1, c1 = 1, 2))
c1$add(2)
ee(unpack(cc), c(1, c1 = 1, 2)) # still the same




# ----------
# S3 methods
# ----------

# an element can be added to the left of a Deque
ee(as.list(addleft(deque(0), 1)), as.list(1:0))
ee(as.list(addleft(deque(mean), median)), list(median, mean))


# number of element occurrences can be counted
names <- deque("Lisa", "Bob", "Bob")
ee(count(names, "Lisa"), 1)
ee(count(names, "Bob"), 2)
ee(count(names, "1"), 0)
ee(count(add(names, "1"), "1"), 1)


# the left and rightmost element can be peeked
v <- 1:3
d <- as.deque(v)
ee(peek(d), utils::tail(v, 1))
ee(peekleft(d), utils::head(v, 1))

# the left and rightmost element can be popped
v <- 1:3
d <- as.deque(v)

ee(pop(d), utils::tail(v, 1))
v <- v[-length(v)]
ee(unlist(as.list(d)), v)
ee(popleft(d), utils::head(v, 1))
v <- v[-1]
ee(unlist(as.list(d)), v)


# popping empty Deques gives an error
expect_error(pop(deque()))
expect_error(popleft(deque()))


# a Deque can be reversed
v <- 1:10
ee(unlist(as.list(rev(as.deque(v)))), rev(v))


# a Deque can be rotated
d <- deque()
ee(d, rotate(d))
add(d, 1)
ee(as.list(d), as.list(rotate(d)))
ee(as.list(rotate(as.deque(1:2))), list(2, 1))
ee(as.list(rotate(as.deque(1:3))), list(3, 1, 2))
ee(as.list(rotate(as.deque(1:3), 2)), list(2, 3, 1))
ee(as.list(rotate(as.deque(1:3), 3)), as.list(1:3))
ee(as.list(rotate(as.deque(1:3), -1)), list(2, 3, 1))
ee(as.list(rotate(as.deque(1:3), -2)), list(3, 1, 2))

v <- rnorm(10)
len <- length(v)
ee(rotate(as.deque(v), 0), rotate(as.deque(v), len))
ee(rotate(as.deque(v), len), rotate(as.deque(v), -len))
ee(rotate(rotate(as.deque(v), 3), -3), as.deque(v))


# Conversion
co <- container(1, 2, 3)
d <- as.deque(co)
ee(as.list(co), as.list(d))
ee(as.deque(NULL), deque())

v <- rnorm(10)
ee(unlist(as.list(as.deque(v))), v)
ee(as.list(d), as.list(d))

