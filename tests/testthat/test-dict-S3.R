context("dict S3")


test_that("basic dict functions work as expected", {
    # initialize
    expect_error(dict(1:2), "all items must be named")
    expect_equal(keys(dict()), character(0))
    d <- dict(c(x=1L, y=2L))
    expect_true(is.dict(d))
    expect_true(is.container(d))
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
    expect_error(add(d, c("x", "y"), 1), "key must be of length 1")
    add(d, "x", 1)
    expect_false(empty(d))
    expect_equal(size(d), 1)
    expect_true(has(d, "x"))
    expect_error(getval(d, "foo"))
    expect_equal(peek(d, "x"), 1)
    expect_equal(peek(d, "foo"), NULL)
    expect_equal(peek(d, "foo", default=0), 0)

    # set and pop
    setval(d, "x", 3)
    expect_equal(size(d), 1)
    expect_equal(d$get("x"), 3)
    expect_equal(pop(d, "x"), 3)
    expect_false(has(d, "x"))

    # keys, discard, delete, popitem
    d <- dict(c(x=1L, y=2L, z=3L))
    expect_output(print(d),
                  '<Dict> of 3 elements:  Named int [1:3] 1 2 3', fixed=TRUE)
    expect_true(has(d, "y"))
    expect_equal(keys(d), c("x", "y", "z"))
    expect_false(has(discard(d, "y"), "y"))
    expect_error(delete(d, "y"), "key 'y' not in Dict")
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

    d1 = dict(list(A=1, B=2, C=3))
    sortkey(d1, decr=TRUE)
    expect_equal(keys(d1), c("C", "B", "A"))
})


test_that("Extract and replace operators work as expected", {
    # `[` and `[[` operators
    d <- dict()
    expect_error(d[["a"]], "key 'a' not in Dict")
    expect_error(d["a"], "key 'a' not in Dict")
    expect_equal(d[["a", default = 1]], 1)
    expect_equal(d["a", default = 1], dict(list(a = 1)))
    d$add("a", 1)$add("b", 2)
    expect_equal(d[["a"]], 1)
    expect_equal(d["b"], dict(list(b = 2)))
    expect_error(d[[c("a", "b")]], "cannot select more than one element")
    expect_equal(as.list(d[c("a", "b")]), list(a=1, b=2))

    # `[[<-` and `[<-` operator
    d <- dict()
    expect_error(d[["a"]] <- 1, "key 'a' not in Dict")
    d[["a", add = TRUE]] <- 1
    expect_equal(d[["a"]], 1)
    d[["a"]] <- 3
    expect_equal(d[["a"]], 3)

    expect_error(d[[c("a", "b")]] <- list(7, 8),
                 "length(key) == 1 is not TRUE", fixed = TRUE)

    expect_error(d[c("a", "b")] <- list(7, 8), "key 'b' not in Dict")
    d$add("b", 2)
    d[c("a", "b")] <- list(7, 8)
    expect_equal(as.list(d), list(a = 7, b = 8))

    expect_error(d["b"] <- NULL, "length of key and value must match")
    d["b"] <- list(NULL) # deletes 'b'
    expect_equal(keys(d), "a")

    d[["a"]] <- NULL
    expect_true(empty(d))
})

test_that("+ and - operators work as expected", {

    # `+` operator
    d1 <- dict(list(a=1, b=2))
    d2 <- dict(list(b=2, c=1))
    dd <- d1 + d2
    expect_equal(d1$size(), 2)
    expect_equal(d2$size(), 2)
    expect_equal(dd$size(), 3)
    expect_equal(dd$values(), d1$update(d2)$values())

    # `-` operator
    d1 <- dict(list(A=1, B=2, C=3))
    d2 <- dict(list(A=1, B=2))
    expect_equal(d1 - d2, dict(list(C=3)))
    expect_equivalent(d2 - d1, dict())
    expect_equivalent(d1 - d1, dict())
    expect_equivalent(dict() - d1, dict())
})


test_that("data.frame can be converted to dict", {
    # as.data.frame
    df <- data.frame(A=1:5, B=1:5)
    d <- dict(df)
    expect_equal(as.data.frame(as.list(d)), df)
})


test_that("multiple elements can be discarded at once", {
    d <- dict(list(A=1, B=2))
    expect_equal(keys(d), c("A", "B"))
    discard(d, keys(d))
    expect_true(empty(d))
})

test_that("multiple elements can be deleted at once", {
    d <- dict(list(A=1, B=2))
    expect_equal(keys(d), c("A", "B"))
    delete(d, keys(d))
    expect_true(empty(d))

    d <- dict(list(A=1, B=2))
    expect_error(delete(d, c("A", "C", "B")), "key 'C' not in Dict")
    expect_equal(keys(d), "B")
})

