ee = expect_equal

# --------------------
# Container [ operator
# --------------------
co = container(a = 1, 2, b = 3, 4)
ee(co[1], container(a = 1))
ee(co[2], container(2))
ee(co["a"], container(a = 1))
ee(container()[1], container())
ee(co[""], container())
ee(co[NULL], container())
ee(co[], co)

ee(co["x"], container())
ee(co[c("x", "y")], container())
ee(co[c("a", "y")], container(a = 1))

ee(co[1:3], as.container(as.list(co)[1:3]))
ee(co[0:5], co)

ee(co[c(1, 1)], c(co[1], co[1]))

ee(co[1, 3], container(a = 1, b = 3))
ee(co[c(1, 3)], container(a = 1, b = 3))
ee(co["b", 1, 4], container(b = 3, a = 1, 4))


# --------------------
# Container [[ operator
# --------------------
co = container(a = 1, 2, b = 3, 4, foo = "bar")

ee(co[[1]], 1)
ee(co[[2]], 2)
ee(co[["a"]], 1)
ee(container()[[1]], NULL)
ee(co[[]], NULL)
ee(co[[""]], NULL)
expect_error(co[[NULL]], "index must be of length 1")

# list inconsistency
l = list(a = 1, 2, e = 3)
ee(l[[]], 2)
ee(l[[""]], NULL)

expect_error(co[[1:2]], "index must be of length 1")
expect_error(co[[c("a", "b")]], "index must be of length 1")
expect_error(co[[NA]], "index must not be 'NA'")

