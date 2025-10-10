describe(
    "Replace operations",
{
    ee <- expect_equal

    describe(
        "Container [<- operator",
    {
        test_that("Container [<- works with numeric indices",
        {
            co <- container(a = 1, b = "bar")
            co[1:2] <- 1:2
            ee(co, container(a = 1, b = 2))
            expect_error(co[3] <- 3, "index out of range")
        })

        test_that("Container [<- works with mixed indices",
        {
            co <- container(a = 1, b = "bar")
            co[list(1, "b")] <- 3:4
            ee(co, container(a = 3, b = 4))

            co[1:2] <- 0
            ee(co, container(a = 0, b = 0))
        })

        test_that("Container [<- handles length mismatches",
        {
            co <- container(1, 2, 3)
            expect_warning(co[1:3] <- 1:2, "number of items to replace")
            ee(co, container(1, 2, 1))
            expect_warning(co[1] <- 5:6, "number of items to replace")
            ee(co, container(5, 2, 1))
        })
    })

    describe(
        "Container [[<- operator",
    {
        test_that("Container [[<- works with numeric indices",
        {
            co <- container(a = 1, b = "bar")
            co[[1]] <- 2
            ee(co, container(a = 2, b = "bar"))
            co[[2]] <- 9
            ee(co, container(a = 2, b = 9))
        })

        test_that("Container [[<- works with character indices",
        {
            co <- container(a = 2, b = 9)
            co[["b"]] <- 0
            co[["x"]] <- 0
            ee(co, container(a = 2, b = 0, x = 0))
            expect_error(co[[4]] <- 1, "index out of range")
        })

        test_that("Container [[<- works with complex replacement patterns",
        {
            co <- container(a = 2, b = 0, x = 0)
            co[[{2}]] <- 1:4
            ee(co, container(a = 1:4, b = 0, x = 0))

            co[[{1:4}]] <- NA
            ee(co, container(a = NA, b = 0, x = 0))

            co[[{NA}]] <- NULL
            ee(co, container(a = NULL, b = 0, x = 0))

            co[[{0}]] <- list(1, "a")
            ee(co, container(a = NULL, b = list(1, "a"), x = 0))

            co[[{list(1, "a")}]] <- 9
            ee(co, container(a = NULL, b = 9, x = 0))
        })

        test_that("Container [[<- validates indices correctly",
        {
            co <- container(a = NULL, b = 9, x = 0)
            expect_error(co[[{1}]] <- 9, "old element \\(1\\) is not in Container")
            expect_error(co[[1, 2]] <- 3:4)
            expect_error(co[[1:2]] <- 3:4, "index must be of length 1")
        })
    })

    describe(
        "Container $<- operator",
    {
        test_that("Container $<- works correctly",
        {
            co <- container(a = 1, b = "bar")
            co$f <- 3
            ee(co, container(a = 1, b = "bar", f = 3))
            co$b <- 2
            ee(co, container(a = 1, b = 2, f = 3))

            co$`x 2` <- 0
            ee(co[["x 2"]], 0)
        })
    })
})
