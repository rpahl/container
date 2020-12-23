context("Iterator")

test_that("Iterator constructor works as expected", {
    expect_error(Iterator$new(environment()), "'x' is not iterable")
    it <- Iterator$new(as.list(environment())) # ok
})

test_that("it can be checked if Iterator has next element", {
    expect_true(Iterator$new(1:5)$has_next())
    expect_false(Iterator$new(list())$has_next())
})

test_that("the next element can be retrieved while incrementing the iterator", {
    it <- Iterator$new(1)
    expect_true(it$has_next())
    expect_equal(it$get_next(), 1)
    expect_false(it$has_next())
})

test_that("the position of the iterator can be accessed", {
    x <- 1:5
    it <- Iterator$new(x)
    expect_equal(it$pos(), 0)
    for (i in x) {
        expect_equal(it$get_next(), i)
        expect_equal(it$pos(), i)
    }
})

test_that("Iterator can be reset", {
    it <- Iterator$new(1:3)
    expect_equal(it$pos(), 0)
    expect_equal(it$get_next(), 1)
    expect_equal(it$pos(), 1)
    it$begin()
    expect_equal(it$pos(), 0)
})

test_that("Iterator can be incremented", {
    x <- 1:5
    it <- Iterator$new(x)
    for (i in x) {
        expect_equal(it$.next()$pos(), i)
    }
})


test_that("Iterator works as expected", {
    s <- "Hello World!"
    s.split <- strsplit(s, split="")[[1]]
    it <- Iterator$new(s.split)
    s2 <- ""
    while(it$has_next()) s2 <- paste0(s2, it$get_next())
    expect_equal(s2, s)
    expect_false(it$has_next())
    expect_error(it$get_next())
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

