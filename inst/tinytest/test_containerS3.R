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

# Ensure container objects are passed as copies as well
coco = container(co)
expect_equal(unpack(coco), 1:2)
co$add(3)
expect_equal(unpack(coco), 1:2)

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

# a dict can be converted to a container
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
expect_equal(ccc1, container(container(container(1))))
expect_equal(l, list(container(container(1))))  # not changed


# -----------------
# c.Container
# -----------------
# concat to empty container
check_c_empty = function(..., recursive = FALSE) {
    cco = c(container(), ..., recursive = recursive)
    if (!recursive)
        cco = as.list(cco)

    cli = c(list(), ..., recursive = recursive)
    expect_equal(cco, cli)
}

check_c_empty(NULL)
check_c_empty(numeric())
check_c_empty(list())
check_c_empty(container())
check_c_empty(list(a = 1, co = container(y = 3, list(z = 4))))
check_c_empty(list(a = 1, co = container(y = 3, list(z = 4))), use.names = FALSE)
check_c_empty(list(a = 1, li = list(y = 3, list(z = 4))))
check_c_empty(list(a = 1, li = list(y = 3, list(z = 4))), recursive = TRUE)

expect_equal(c(container(), list(a = 1, li = container(y = 3, dict(z = 4))), recursive = TRUE),
             c(     list(), list(a = 1, li =      list(y = 3, list(z = 4))), recursive = TRUE))

expect_equal(as.list(c(container(a = 1, list(x = 9)), list(b = 2, li = list(), NULL))),
                     c(     list(a = 1, list(x = 9)), list(b = 2, li = list(), NULL)))

expect_equal(c(container(a = 1, list(x = 9)), list(b = 2, li = list(), NULL), recursive = TRUE),
                  c(list(a = 1, list(x = 9)), list(b = 2, li = list(), NULL), recursive = TRUE))


# Ensure concatenated objects are always copies
c1 = container(1)
c2 = container(2)
c1c1 = container(c1 = c1)

cc = c(c1, c1c1, c2)
expect_equal(unpack(cc), c(1, c1 = 1, 2))
c1$add(2)
expect_equal(unpack(cc), c(1, c1 = 1, 2)) # still the same



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

