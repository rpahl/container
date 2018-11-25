context("Iterator")

test_that("Iterator", {
    s <- "Hello World!"
    s.split <- strsplit(s, split="")[[1]]
    it <- Iterator$new(s.split)
    s2 <- ""
    while(it$has.next()) s2 <- paste0(s2, it$get.next())
    expect_equal(s2, s)
    expect_false(it$has.next())
    expect_error(it$get.next())

    # Iterator from Container
    v <- 1:5
    co <- Container$new(v)
    it <- co$iterator()
    sum <- 0
    while(it$has.next()) sum <- sum + it$get.next()
    expect_equal(sum(v), sum(co$values()))
    expect_equal(sum, sum(co$values()))
})

test_that("Iterator S3 interface", {
    s <- "Hello World!"
    s.split <- strsplit(s, split="")[[1]]
    it <- Iterator$new(s.split)
    s2 <- ""
    while(it$has.next()) s2 <- paste0(s2, it$get.next())
    expect_equal(s2, s)
    expect_false(it$has.next())
    expect_error(it$get.next())

    # Iterator from Container
    v <- 1:5
    co <- Container$new(v)
    it <- co$iterator()
    sum <- 0
    while(it$has.next()) sum <- sum + it$get.next()
    expect_equal(sum(v), sum(co$values()))
    expect_equal(sum, sum(co$values()))
})

