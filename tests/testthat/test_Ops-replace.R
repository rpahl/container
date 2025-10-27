describe(
    "Replace operations",
{
    ee <- function(...) {
        expect_true(isTRUE(all.equal(..., check.attributes = FALSE)))
    }

    describe(
        "Container [<- operator",
    {
        test_that("it handles empty input like lists",
        {
            co <- container(a = 1, b = "bar")
            l <- as.list(co)

            ee(co[] <- 0, l[] <- 0)
            ee(co[integer()] <- 1, l[integer()] <- 1)

        })
        test_that("empty or undefined indices",
        {
            co <- container(a = 1, b = 2)

            ee(co[], co)
            ee(co[""], container())
            ee(container()[1], container())

            ee(co[integer()], container())
            ee(co[numeric()], container())
            ee(co[character()], container())
            ee(co[logical()], container())

            ee(co[NA_character_], container())
            ee(co[NA_real_], container())
            expect_warning(
                ee(co[NA], container()),
                "Logical index contains NA; treating NA as FALSE."
            )

            # consistency to list behavior for NULL index
            l <- as.list(co)

            expect_equal(as.list(co[NULL]), unname(l[NULL]))
            expect_equal(as.list(co[i = NULL]), unname(l[i = NULL]))
            expect_equal(as.list(co[foo = NULL]), unname(l[foo = NULL]))
        })

        test_that("works with numeric indices",
        {
            co <- container(a = 1, b = "bar")
            co[1:2] <- 1:2
            ee(co, container(a = 1, b = 2))
            expect_error(co[3] <- 3, "index out of range")
        })

        test_that("works with mixed indices",
        {
            co <- container(a = 1, b = "bar")
            co[list(1, "b")] <- 3:4
            ee(co, container(a = 3, b = 4))

            co[1:2] <- 0
            ee(co, container(a = 0, b = 0))
        })

        test_that("works with negative indices",
        {
            co <- container(a = 1, b = 2, c = 3, d = 4)
            co[-c(1, 3)] <- 9:10
            ee(co, container(a = 1, b = 9, c = 3, d = 10))
        })

        test_that("works with logical indices",
        {
            co <- container(a = 1, b = 2, c = 3, d = 4)
            co[c(TRUE, FALSE, TRUE, FALSE)] <- c(9, 8)
            ee(co, container(a = 9, b = 2, c = 8, d = 4))

            mask <- c(TRUE, FALSE)
            co <- container(a = 1, b = 2, c = 3, d = 4)
            co[mask] <- 5
            ee(co, container(a = 5, b = 2, c = 5, d = 4))
        })

        test_that("works with NSE alphanumeric ranges",
        {
            co <- container(a = 1, b = "bar")
            co[a:b] <- 3:4
            ee(co, container(a = 3, b = 4))

            co <- container(a = 1, 2, b = 3, d = 4)
            co[a:2] <- 7:8
            ee(co, container(a = 7, 8, b = 3, d = 4))

            co <- container(a = 1, b = 2, c = 3, d = 4)
            co[-(a:c)] <- 9
            ee(co, container(a = 1, b = 2, c = 3, d = 9))
        })

        test_that("adds unknown character indices",
        {
            co <- container(a = 1, b = 2)
            co["x"] <- 7
            ee(co, container(a = 1, b = 2, x = 7))
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
