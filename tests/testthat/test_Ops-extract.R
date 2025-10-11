
describe(
    "[ operator",
{
    ee <- function(...) expect_true(isTRUE(all.equal(..., check.attributes = FALSE)))

    test_that("empty or undefined indices",
    {
        co <- container(a = 1, b = 2)

        ee(co[], co)
        ee(co[""], container())
        ee(container()[1], container())
        ee(co[NULL], container())   #fails

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
    })


    test_that("numeric indices",
    {
        co <- container(a = 1, 2, b = 3, 4)

        # Single
        ee(co[0], container())
        ee(co[0, strict = TRUE], container())
        ee(co[1], container(a = 1))
        ee(co[2], container(2))
        ee(co[6], container())
        expect_error(co[6, strict = TRUE], "Out-of-bounds indices: 6")

        # Multiple
        ee(co[0:5], co)
        ee(co[1, 3], container(a = 1, b = 3))
        ee(co[1, 2, 3], co[1:3])
        ee(co[1:3], as.container(as.list(co)[1:3]))

        # Duplicates
        ee(co[c(1, 1)], c(co[1], co[1]))
        ee(co[1, 2, 1], c(co[1], co[2], co[1]))

        # Negative
        ee(co[-1], container(2, b = 3, 4))
        ee(co[-c(1, 3)], container(2, 4))
        ee(co[-1, -3], container(2, 4))
    })

    test_that("character indices",
    {
        co <- container(a = 1, 2, b = 3, 4)

        # Single
        ee(co[""], container(2))
        ee(co["a"], container(a = 1))
        ee(co["b"], container(b = 3))
        ee(co["x"], container())
        expect_error(co["x", strict = TRUE], "Unknown names in index: \"x\"")

        # Multiple
        ee(co[c("a", "b")], container(a = 1, b = 3))
        ee(co[c("a", "x")], container(a = 1))

        # Duplicates
        ee(co[c("a", "a")], c(co["a"], co["a"]))
        ee(co[c("a", "b", "a")], c(co["a"], co["b"], co["a"]))

        # Negative
        ee(co[-c("a")], co[2:4])
        ee(co[-"a"], co[2:4])
        ee(co[-c("a", "b")], container(2, 4))
        ee(co[-"a", -"b"], container(2, 4))
    })

    test_that("boolean indices",
    {
        co <- container(a = 1, b = 2, c = 3, d = 4)
        co <- container(a = 1, 2, b = 3, 4)

        l <- as.list(co)
        ee(as.list(co[TRUE]), l[TRUE])
        ee(as.list(co[FALSE]), unname(l[FALSE]))
        ee(as.list(co[TRUE, FALSE]), l[c(TRUE, FALSE)]) # fails

        m2 <- c(TRUE, FALSE)
        ee(as.list(co[m2]), l[m2])
        ee(as.list(co[rev(m2)]), l[rev(m2)])

        m3 <- c(TRUE, TRUE, FALSE)
        expect_warning(
            expect_equal(as.list(co[m3]), l[m3]),
            "Logical index of length 3 is not a multiple of 4; recycling"
        )
        expect_warning(
            expect_equal(
                as.list(co[rev(m3)]), l[rev(m3)]
            ),
            "Logical index of length 3 is not a multiple of 4; recycling"
        )
    })

    test_that("mixed indices",
    {
        co <- container(a = 1, 2, b = 3, 4)

        ee(co["b", 1, 4], container(b = 3, a = 1, 4))
        ee(co[list("b", 1, 4)], co["b", 1, 4])
    })

    test_that("indices passed as variables",
    {
        co <- container(a = 1, b = 2, c = 3, d = 4)
        numbers <- 1:3
        co[numbers]
        ee(co[numbers], co[1:3])
        varNames <- c("a", "c")
        ee(co[varNames], co[c(1, 3)])
        ee(co[numbers, varNames], co[1:3, c(1, 3)])

        a <- 1:3
        ee(co[a], co[1])
    })

    test_that("Alphabetic range indices (NSE)",
    {
        co <- container(a = 1, b = 2, c = 3, d = 4)
        ee(co[a:d], co[1:4])
        ee(co[a:b], co[1:2])
        ee(co[a:b, d:c], co[1:2, 4:3])
        co <- container("a 1" = 1, 2, "a 2" = 3, d = 4)
        ee(co[`a 1`:`a 2`], co[1:3])

        # Mixed numeric/alphabetic ranges
        co <- container(a = 1, b = 2, c = 3, d = 4)
        ee(co[a:3], co[1:3])
        ee(co[a:b, 4:3], co[1:2, 4:3])
        ee(co[a:2, 4:c], co[1:2, 4:3])
    })

    test_that("consistency to list behavior for NULL index",
    {
        co <- container(a = 1, b = 2)
        l <- as.list(co)

        expect_equal(as.list(co[NULL]), unname(l[NULL]))
        expect_equal(as.list(co[a = NULL]), unname(l[a = NULL]))
        expect_equal(as.list(co[x = NULL]), unname(l[x = NULL]))
    })

    test_that(".default argument",
    {
        # Default values
        co <- container(a = 1, 2, b = 3, 4)
        ee(co[1, 99, .default = 0], container(a = 1, 0))
        ee(co[1:2, 11:12, .default = 0], c(co[1:2], 0, 0))
        ee(co[1:2, "z", .default = 0], c(co[1:2], z = 0))
        ee(co[1:2, "z", .default = 3:4], container(a = 1, 2, z = c(3L, 4L)))
    })
})


describe(
    "[[ operator",
{
    ee <- expect_equal

    test_that("empty or undefined indices",
    {
        co <- container(a = 1, 2, b = 3, 4, b = "bar")

        ee(co[[1]], 1)
        ee(co[[2]], 2)
        ee(co[["a"]], 1)
        ee(container()[[1]], NULL)
        ee(co[[]], NULL)
        ee(co[[0]], NULL)
        ee(co[[""]], NULL)
        ee(co[[6]], NULL)
        ee(co[[NULL]], NULL)
        ee(co[[integer(0)]], NULL)
    })

    test_that("consistency to list behavior for certain edge cases",
    {
        co = container(a = 1, b = 2)
        l = as.list(co)
        ee(co[[""]], l[[""]])
        expect_error(co[[1:2]], "index must be of length 1")
        expect_error(co[[c("a", "b")]], "index must be of length 1")
        expect_error(co[[NA]], "index must not be 'NA'")

        skip("TODO: currently not equal (NULL vs 1)")
        #ee(co[[TRUE]], l[[TRUE]]) # nolint
    })
})
