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
    itSum <- 0
    while(it$has_next()) itSum <- itSum + it$get_next()
    expect_equal(sum(v), sum(co$values()))
    expect_equal(itSum, sum(co$values()))
})

