ee <- expect_equal

local({
    # --------------------
    # Container [ operator
    # --------------------
    co <- container(a = 1, 2, b = 3, 4)
    ee(co[1], container(a = 1))
    ee(co[2], container(2))
    ee(co["a"], container(a = 1))
    ee(container()[1], container())
    ee(co[""], container())
    ee(co[NULL], container())
    ee(co[integer(0)], container())
    ee(co[0], container())
    ee(co[6], container())
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
    ee(co[list("b", 1, 4)], co["b", 1, 4])

    # Alphabetic range indices
    co <- container(a = 1, b = 2, c = 3, d = 4)
    ee(co[a:d], co[1:4])
    ee(co[a:b], co[1:2])
    ee(co[a:b, d:c], co[1:2, 4:3])
    co <- container("a 1" = 1, 2, "a 2" = 3, d = 4)
    ee(co[`a 1`:`a 2`], co[1:3])

    # Mixed numeric/alphabetic ranges
    co <- container(a = 1, b = 2, c = 3, d = 4)
    ee(co[a:3], co[1:3])
    ee(co[a:b, 4:3], co[1:2, 4:3])
    ee(co[a:2, 4:c], co[1:2, 4:3])

    # Boolean indexing
    co[T, F, T, F]
    co[c(T, F, T, F)]

    # Make sure list behavior is mimicked if empty arg
    ee(as.list(co[]), as.list(co)[])

    # Default values
    co <- container(a = 1, 2, b = 3, 4)
    ee(co[1, 99, .default = 0], container(a = 1, 0))
    ee(co[1:2, 11:12, .default = 0], c(co[1:2], 0, 0))
    ee(co[1:2, "z", .default = 0], c(co[1:2], z = 0))
    ee(co[1, 99, .default = b], container(a = 1, co[["b"]]))
    ee(co[1, "z", .default = b], container(a = 1, z = co[["b"]]))
})


# --------------------
# Container [[ operator
# --------------------
co <- container(a = 1, 2, b = 3, 4, b = "bar")

ee(co[[1]], 1)
ee(co[[2]], 2)
ee(co[["a"]], 1)
ee(container()[[1]], NULL)
ee(co[[]], NULL)
ee(co[[0]], NULL)
ee(co[[""]], NULL)
ee(co[[6]], NULL)
ee(co[[NULL]], NULL)
ee(co[[integer(0)]], NULL)

# list inconsistency
l <- list(a = 1, 2, e = 3)
ee(l[[""]], NULL)

expect_error(co[[1:2]], "index must be of length 1")
expect_error(co[[c("a", "b")]], "index must be of length 1")
expect_error(co[[NA]], "index must not be 'NA'")
