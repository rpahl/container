# ---------
# container
# ---------
expect_equal(container(), Container$new())
expect_equal(container(NULL), Container$new(NULL))
expect_equal(container(NA), Container$new(NA))
expect_equal(container(numeric(0)), Container$new(numeric(0)))
expect_equal(container(list()), Container$new(list()))
expect_equal(container(1, 2, NULL), Container$new(1, 2, NULL))
co = container(1, 2)
expect_equal(container(co), Container$new(co))

# ------------
# as.container
# ------------
expect_equal(as.container(numeric()), container())
expect_equal(as.container(NULL), container())
expect_equal(as.container(list()), container())
expect_equal(as.container(1), container(1))
expect_equal(as.container(1:2), container(1, 2))
expect_equal(as.container(container(1)), container(1))

# container can be created as copy from another container
co = container(1, 2)
co2 = as.container(co)
expect_equal(co, co2)
co$clear()
expect_equal(length(co), 0)
expect_equal(length(co2), 2)

# a data.frame can be converted to a container
daf = data.frame(A = 1:2, B = 3:4)
expect_equal(as.list(as.container(daf)), as.list(daf))

# a set can be converted to a container
s = setnew(1, 2)
expect_equal(as.list(as.container(s)), list(1, 2))

# a deque can be converted to a container
d = deque(1, 2)
expect_equal(as.list(as.container(d)), list(1, 2))

d = dict(a = 1, b = 2)
expect_equal(as.list(as.container(d)), list(a = 1, b = 2))

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
expect_equal(as.list(as.container(l)), l)

# Ensure nested containers are always converted as copies
c1 = container(1)
cc1 = container(c1)
ccc1 = container(cc1)
l = as.list(ccc1)
expect_equal(l, list(container(container(1))))
c1$add(2)
cc1$add(2)
expect_equal(ccc1, container(container(container(1, 2), 2)))
expect_equal(l, list(container(container(1))))  # not changed


# -----------------
# c.Container
# -----------------
# Standard concatenate
c1 = container(1)
expect_equal(c(c1, NULL), c1)
expect_equal(c(c1, list()), c1)
expect_equal(c(c1, numeric()), c1)
expect_equal(c(c1, 2:3), as.container(1:3))
c2 = container(2)
expect_equal(c(c1, c2, c2), container(1, 2, 2))

# Ensure concatenated objects are always copies also for nested containers
cc1 = container(c1)
ccc1 = container(cc1)

cc = c(c1, cc1, ccc1)
expect_equal(cc, container(1, container(1), container(container(1))))
c1$add(3)
cc1$add(3)
ccc1$add(3)
# If copies were concatenated the following must still hold
expect_equal(cc, container(1, container(1), container(container(1))))


# ----------------
# length.Container
# ----------------
expect_equal(length(container()), 0)
expect_equal(length(container(1)), 1)
expect_equal(length(container(NULL)), 1)
expect_equal(length(container(1, as.container(1:10))), 2)

# ---------------
# names.Container
# ---------------
expect_equal(names(container()), NULL)
expect_equal(names(container(numeric())), NULL)
expect_equal(names(container(list())), NULL)
expect_equal(names(container(1, 2, 3)), NULL)
expect_equal(names(container(a = 1, 2, x = 5)), c("a", "", "x"))


# ------
# unpack
# ------
expect_equal(unpack(container()), NULL)
expect_equal(unpack(container(NULL)), NULL)
expect_equal(unpack(container(container())), NULL)
expect_equal(unpack(container(container(list()))), NULL)
expect_equal(unpack(list()), NULL)
expect_equal(unpack(list(list())), NULL)
expect_equal(unpack(list(list(1))), 1)
expect_equal(unpack(list(1:2)), 1:2)
expect_equal(unpack(container(numeric())), numeric())
expect_equivalent(unpack(container(data.frame(A = 1:2, B = 3:4))), 1:4)

co <- container(1, mean)
co2 <- container(2, co, co)
expect_equal(unpack(co2), unlist(list(2, as.list(co), as.list(co))))
l <- list(co, list(list(3, 4), co2))
expect_equal(unpack(l), list(1, mean, 3, 4,  2, 1, mean, 1, mean))

co3 <- container(7, 6)
l <- list(list(container(co3,  co3), 0), 1:4)
expect_equal(unpack(l), c(7,6,  7,6,  0,  1:4))


