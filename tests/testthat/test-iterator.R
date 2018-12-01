context("Iterator")

test_that("Iterator", {
    s <- "Hello World!"
    s.split <- strsplit(s, split="")[[1]]
    it <- Iterator$new(s.split)
    s2 <- ""
    while(it$has_next()) s2 <- paste0(s2, it$get_next())
    expect_equal(s2, s)
    expect_false(it$has_next())
    expect_error(it$get_next())

    # Iterator from Container
    v <- 1:5
    co <- Container$new(v)
    it <- co$iter()
    sum <- 0
    while(it$has_next()) sum <- sum + it$get_next()
    expect_equal(sum(v), sum(co$values()))
    expect_equal(sum, sum(co$values()))
})

test_that("Iterator S3 interface", {
    s <- "Hello World!"
    s.split <- strsplit(s, split="")[[1]]
    it <- iter(s.split)
    s2 <- ""
    while(ithas_next(it)) s2 <- paste0(s2, itget_next(it))
    expect_equal(s2, s)
    expect_false(ithas_next(it))
    expect_error(itget_next(it))

    # Iterator from Container
    v <- 1:5
    co <- container(v)
    it <- iter(co)
    sum <- 0
    while(ithas_next(it)) sum <- sum + itget_next(it)
    expect_equal(sum(v), sum(values(co)))
    expect_equal(sum, sum(values(co)))
})

