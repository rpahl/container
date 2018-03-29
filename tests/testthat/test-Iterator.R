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
    vSum <- 0
    while(it$has_next()) vSum <- vSum + it$get_next()
    expect_equal(vSum, sum(v))

    # Apply
    co <- Container$new(as.numeric(v))
    expect_equal(unlist(co$apply(f = base::log)), log(v))
})

