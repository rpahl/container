# ------
# unpack
# ------
# Verify equality to unlist if only using list arguments
check = function(x, ...) {
    expect_equal(unpack(x, ...), unlist(x, ...))
}

check(NULL)
check(numeric())
check(list())
check(c(a = 1))
check(c(a = 1), use.names = FALSE)
check(list(a = 1))
check(list(a = 1, 2))
check(list(a = 1, 2, use.names = FALSE))

l = list(a = list(b = 1:5, ll = list(0, x = NULL, 1)))
check(l)
check(l, recursive = FALSE)
check(l, use.names = FALSE)
check(l, recursive = FALSE, use.names = FALSE)


# container vs list
check = function(x, y, ...) {
    expect_equal(unpack(x, ...), unlist(y, ...))
}

check(container(), list())
check(container(), list(), recursive = FALSE)
check(container(NULL), list(NULL))
check(container(container()), list(list()))
check(container(co = container()), list(co = container()), recursive = FALSE)

daf = data.frame(A = 1:2, B = 3:4)
check(container(daf = daf), list(daf = daf))
check(container(daf = daf), list(daf = daf), use.names = FALSE)
check(container(daf = daf), list(daf = daf), recursive = FALSE)

co <- container(a = 1, b = 2)
check(container(co = co), list(co = co), recursive = FALSE)
expect_equal(unpack(container(co = co)), c(co.a = 1, co.b = 2))


# nested containers
co <- container(co = container(0),
                s = setnew(1, 2),
                d = dict(a = 1, b = 9),
                de = as.deque(5:6))
expect_equal(unpack(co),
             c(co = 0, s1 = 1, s2 = 2, d.a = 1, d.b = 9, de1 = 5, de2 = 6))


