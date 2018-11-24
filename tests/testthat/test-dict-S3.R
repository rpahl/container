context("dict S3 methods")

test_that("General", {
    d <- Dict$new(list(A=1, B=2))
    expect_true(is.dict(d))
    expect_true(is.dict(as.dict(list())))
    expect_true(is.container(d))
    expect_output(print(d), "<Dict> of 2 elements: List of 2")
    expect_equal(as.list(d), d$values())
    expect_equal(as.vector(d), d$values())

    # as.data.frame
    df <- data.frame(A=1:5, B=1:5)
    d <- dict(df)
    expect_equal(as.data.frame(d), df)
    expect_equal(as.data.frame(dict()), data.frame())
})

test_that("Operators", {
    d <- Dict$new()

    # `[<-` operator
    expect_error(d["a"] <- 1, "key 'a' not in Dict")
    d["a", add=TRUE] <- 1
    expect_equal(d$get("a"), 1)
    d["a"] <- 3
    expect_equal(d$get("a"), 3)
    d["b", add=TRUE] <- d["a"]
    expect_equal(d$get("a"), d$get("b"))

    # `[` and `[[` operators
    expect_equal(d$get("a"), d["a"])
    expect_equal(d$peek("a"), d[["a"]])
    expect_error(d["z"], "key 'z' not in Dict")
    expect_true(is.null(d[["z"]]))
    expect_equal(d$peek("z", default=1), d[["z", default=1]])

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


test_that("Member functions", {
    expect_equal(add(dict(), "a", 1), Dict$new()$add("a", 1))
    ll <- list(A=1, B=2, C=3)
    d <- dict(ll)
    discard(d, "A")
    expect_false(has(d, "A"))
    expect_true(has(d, "B"))
    expect_equal(as.list(d), ll[-1])
    expect_equal(as.list(discard(d, "D")), ll[-1])
    expect_error(remove(d, "A"), "key 'A' not in Dict")
    expect_error(set(d, "A", 7), "key 'A' not in Dict")
    expect_equal(as.list(set(d, "A", 1, add=TRUE)$sort()), ll)
})

