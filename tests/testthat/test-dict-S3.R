context("dict S3")


test_that("dict", {
    # initialize
    expect_error(dict(1:2), "all items must be named")
    expect_equal(keys(dict()), character(0))
    d <- dict(c(x=1L, y=2L))
    expect_true(is.dict(d))
    expect_true(is.container(d))
    expect_error(d[["z"]], "key 'z' not in Dict")
    expect_error(add(d, key="", 3L, "zero-length key"))
    expect_equal(as.integer(values(d)), 1:2)
    expect_equal(type(d), "integer")
    expect_equal(type(dict()), "list")
    expect_error(dict(list(x=1, y=2, x=3)), "duplicated keys")

    # empty, size, has, add, peek and getval
    d <- dict()
    expect_equal(attr(d, "name"), "<Dict>")
    expect_true(empty(d))
    expect_error(add(d, key=1, 1), "key must be character")
    expect_error(add(d, c("x", "y"), 1), "key must be single character string")
    add(d, "x", 1)
    expect_false(empty(d))
    expect_equal(size(d), 1)
    expect_true(has(d, "x"))
    expect_error(getval(d, "foo"))
    expect_equal(peek(d, "x"), 1)
    expect_equal(peek(d, "foo"), NULL)
    expect_equal(peek(d, "foo"), d["foo"])
    expect_equal(peek(d, "foo", default=0), 0)
    expect_equal(peek(d, "foo", default=0), d["foo", default=0])

    # set and pop
    setval(d, "x", 3)
    expect_equal(size(d), 1)
    expect_equal(d["x"], 3)
    expect_equal(pop(d, "x"), 3)
    expect_false(has(d, "x"))

    # keys, discard, remove, popitem
    d <- dict(c(x=1L, y=2L, z=3L))
    expect_output(print(d),
                  '<Dict> of 3 elements:  Named int [1:3] 1 2 3', fixed=TRUE)
    expect_true(has(d, "y"))
    expect_equal(keys(d), c("x", "y", "z"))
    expect_false(has(discard(d, "y"), "y"))
    expect_error(remove(d, "y"), "key 'y' not in Dict")
    expect_false(has(discard(d, "y"), "y")) # no error although not in Dict
    expect_error(setval(d, "y", 10), "key 'y' not in Dict")
    setval(d, "y", 10, add=TRUE)
    expect_true(has(d, "y"))

    v <- values(d) # x=1, z=3, y=10
    v2 <- vector(mode = type(d))
    set.seed(123)
    for(i in seq_len(size(d))) {
        v2 <- c(v2, popitem(d))
    }

    expect_true(empty(d))
    expect_error(popitem(d), "pop at empty Dict")
    expect_true(setequal(v, v2))
    expect_equal(names(sort(v)), names(sort(v2)))

    d1 <- dict(list(A=1, B=2))
    d2 <- dict(list(     B=7, C=3))
    update(d1, d2)
    expect_equal(d1, dict(list(A=1, B=7, C=3)))

    sortkey(d1, decr=TRUE)
    expect_equal(keys(d1), c("C", "B", "A"))
})


test_that("Operators", {
    d <- dict()

    # `[[<-` and `[<-` operator
    expect_error(d[["a"]] <- 1, "key 'a' not in Dict")
    d[["a", add=TRUE]] <- 1
    expect_equal(d$get("a"), 1)
    d[["a"]] <- 3
    expect_equal(d$get("a"), 3)
    d["b"] <- 3
    expect_equal(d[["b"]], 3)

    # `[` and `[[` operators
    expect_equal(d$get("a"), d["a"])
    expect_equal(d$peek("a"), d["a"])
    expect_error(d[["z"]], "key 'z' not in Dict")
    expect_true(is.null(d["z"]))
    expect_equal(d$peek("z", default=1), d["z", default=1])

    # `+` operator
    d2 <- dict(list(b=2, c=1))
    dd <- d + d2
    expect_equal(d$size(), 2)
    expect_equal(d2$size(), 2)
    expect_equal(dd$size(), 3)
    expect_equal(dd$values(), d$update(d2)$values())

    # `-` operator
    d1 <- dict(list(A=1, B=2, C=3))
    d2 <- dict(list(A=1, B=2))
    expect_equal(d1 - d2, dict(list(C=3)))
    expect_equivalent(d2 - d1, dict())
    expect_equivalent(d1 - d1, dict())
    expect_equivalent(dict() - d1, dict())
})


test_that("Conversion", {
    # as.data.frame
    df <- data.frame(A=1:5, B=1:5)
    d <- dict(df)
    expect_equal(as.data.frame(d), df)
    expect_equal(as.data.frame(dict()), data.frame())
    expect_equal(as.vector(container()), list())
})

