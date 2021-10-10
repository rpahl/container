ee = expect_equal

# ---------
# container
# ---------
ee(container(), Container$new())
ee(container(NULL), Container$new(NULL))
ee(container(NA), Container$new(NA))
ee(container(numeric(0)), Container$new(numeric(0)))
ee(container(list()), Container$new(list()))
ee(container(1, 2, NULL), Container$new(1, 2, NULL))
co = container(1, 2)
ee(container(co), Container$new(co))

# Ensure container objects are passed as copies as well
coco = container(co)
ee(unpack(coco), 1:2)
co$add(3)
ee(unpack(coco), 1:2)

# ------------
# as.container
# ------------
ee(as.container(numeric()), container())
ee(as.container(NULL), container())
ee(as.container(list()), container())
ee(as.container(1), container(1))
ee(as.container(1:2), container(1, 2))
ee(as.container(container(1)), container(1))

# container is created as copy from another container
co = container(1, 2)
co2 = as.container(co)
ee(as.container(co), as.cont(co))
ee(co, co2)
co$clear()
ee(length(co), 0)
ee(length(co2), 2)

ee(container(1, 2), cont(1, 2))

# a data.frame can be converted to a container
daf = data.frame(A = 1:2, B = 3:4)
ee(as.list(as.container(daf)), as.list(daf))

# a set can be converted to a container
s = setnew(1, 2)
ee(as.list(as.container(s)), list(1, 2))

# a deque can be converted to a container
d = deque(1, 2)
ee(as.list(as.container(d)), list(1, 2))

# a dict can be converted to a container
d = dict(a = 1, b = 2)
ee(as.list(as.container(d)), list(a = 1, b = 2))

# ------------
# is.container
# ------------
expect_error(is.container())
expect_false(is.container(0))
expect_false(is.container(list()))

expect_true(is.container(container()))
expect_true(is.container(container(NULL)))
expect_true(is.container(setnew()))
expect_true(is.container(dict()))
expect_true(is.container(deque()))

# -----------------
# as.list.Container
# -----------------
expect_true(is.list(as.list(container())))

co = container(a = 1, 2)
l = list(1, numeric(0), NULL, co)
ee(as.list(as.container(l)), l)

# Ensure nested containers are always converted as copies
c1 = container(1)
cc1 = container(c1)
ccc1 = container(cc1)
l = as.list(ccc1)
ee(l, list(container(container(1))))
c1$add(2)
cc1$add(2)
ee(ccc1, container(container(container(1))))
ee(l, list(container(container(1))))  # not changed


# -----------
# c.Container
# -----------
# standard non-recursive
ee(as.list(c(container())), c(list()))
ee(as.list(c(container(1))), c(list(1)))
ee(as.list(c(container(NULL))), c(list(NULL)))

ee(as.list(c(container(), container())),
           c(     list(),      list()))
ee(as.list(c(container(1), container())),
                     c(     list(1),      list()))
ee(as.list(c(container(1), container(2))),
           c(     list(1),      list(2)))
ee(as.list(c(container(1), container(2, list(a = 3)))),
           c(     list(1),      list(2, list(a = 3))))
ee(as.list(c(container(1), container(2, container(a = 3)))),
           c(     list(1),      list(2, container(a = 3))))
ee(c(container(1), dict(a = 2, b = container(a = 3))),
     container(1,       a = 2, b = container(a = 3)))

ee(c(container(1), dict(a = 2, b = container(a = 3)), use.names = FALSE),
     container(1,           2,     container(a = 3)))
ee(as.list(c(a = container(1), b = container(2, list(a = 3)), use.names = FALSE)),
           c(         list(1),          list(2, list(a = 3)), use.names = FALSE))


# recursive
cr = function(...) c(..., recursive = TRUE)
ee(cr(container()),
   cr(     list()))
ee(cr(container(1)),
   cr(     list(1)))
ee(cr(container(NULL)),
   cr(     list(NULL)))

ee(cr(container(), container()),
   cr(     list(),      list()))
ee(cr(container(1), container()),
   cr(     list(1),      list()))
ee(cr(container(1), container(2)),
   cr(     list(1),      list(2)))
ee(cr(container(1), container(2, 3)),
   cr(     list(1),      list(2, 3)))
ee(cr(container(1), container(2, list(a = 3))),
   cr(     list(1),      list(2, list(a = 3))))
ee(cr(container(1), container(2, container(a = 3))),
   cr(     list(1),      list(2, list(a = 3))))
ee(cr(container(1),      list(2, container(a = 3))),
   cr(     list(1),      list(2, list(a = 3))))
ee(cr(container(1),      list(2, dict(a = 3))),
   cr(     list(1),      list(2, list(a = 3))))
ee(cr(container(),       list(2, dict(a = 3))),
   cr(     list(),       list(2, list(a = 3))))

ee(c(container(1), dict(a = 2, b = container(a = 3)), recursive = TRUE),
   c(1, a = 2, b.a = 3))


# Ensure concatenated objects are always copies
c1 = container(1)
c2 = container(2)
c1c1 = container(c1 = c1)

cc = c(c1, c1c1, c2)
ee(unpack(cc), c(1, c1 = 1, 2))
c1$add(2)
ee(unpack(cc), c(1, c1 = 1, 2)) # still the same


# ----------------
# length.Container
# ----------------
ee(length(container()), 0)
ee(length(container(1)), 1)
ee(length(container(NULL)), 1)
ee(length(container(1, as.container(1:10))), 2)

# ---------------
# names.Container
# ---------------
ee(names(container()), NULL)
ee(names(container(numeric())), NULL)
ee(names(container(list())), NULL)
ee(names(container(1, 2, 3)), NULL)
ee(names(container(a = 1, 2, x = 5)), c("a", "", "x"))

# ---------------
# str.Container
# ---------------
co = container(1:3, container("a", 1))
out = utils::capture.output(str(co))

expected_out = c("Container of 2 ",
                 " $ : int [1:3] 1 2 3",
                 " $ :Container of 2 ",
                 "  ..$ : chr \"a\"",
                 "  ..$ : num 1")

expect_equal(out, expected_out)

