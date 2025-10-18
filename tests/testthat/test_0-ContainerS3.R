
describe(
    "container",
{
    ee <- expect_equal

    test_that("it creates the expected Container objects",
    {
        ee(container(), Container$new())
        ee(container(NULL), Container$new(NULL))
        ee(container(NA), Container$new(NA))
        ee(container(numeric(0)), Container$new(numeric(0)))
        ee(container(list()), Container$new(list()))
        ee(container(1, 2, NULL), Container$new(1, 2, NULL))
        co <- container(1, 2)
        ee(container(co), Container$new(co))
    })

    test_that("ensure container objects are passed as copies as well",
    {
        co <- container(1, 2)
        coco <- container(co)
        ee(unpack(coco), 1:2)

        co$add(3)
        ee(unpack(coco), 1:2)
    })
})


describe(
    "cont",
{
    ee <- expect_equal

    test_that("cont can be used as a shortcut for container",
    {
        ee(cont(), container())
        ee(cont(NULL), container(NULL))
        ee(cont(NA), container(NA))
        ee(cont(numeric(0)), container(numeric(0)))
        ee(cont(list()), container(list()))
        ee(cont(1, 2, NULL), container(1, 2, NULL))
        co <- cont(1, 2)
        ee(cont(co), container(co))
    })
})


describe(
    "as.container",
{
    ee <- expect_equal

    test_that("as.container conversion works as expected",
    {
        ee(as.container(numeric()), container())
        ee(as.container(NULL), container())
        ee(as.container(list()), container())
        ee(as.container(1), container(1))
        ee(as.container(1:2), container(1, 2))
        ee(as.container(container(1)), container(1))
    })

    test_that("containers are created as independent copies",
    {
        co <- container(1, 2)
        co2 <- as.container(co)

        ee(as.container(co), as.cont(co))
        ee(co, co2)

        co$clear()
        ee(length(co), 0)
        ee(length(co2), 2)
        ee(container(1, 2), cont(1, 2))
    })

    test_that("a data.frame can be converted to a container",
    {
        daf <- data.frame(A = 1:2, B = 3:4)
        ee(as.container(daf), container(A = 1:2, B = 3:4))
    })

    test_that("a set can be converted to a container",
    {
        s <- setnew(1, 2)
        ee(as.container(s), container(1, 2))
    })

    test_that("a deque can be converted to a container",
    {
        d <- deque(1, 2)
        ee(as.container(d), container(1, 2))
    })

    test_that("a dict can be converted to a container",
    {
        d <- dict(a = 1, b = 2)
        ee(as.container(d), container(a = 1, b = 2))
    })

    test_that("a named vector can be converted to a container",
    {
        nv <- c(a = 1, b = 2)
        ee(as.container(nv), container(a = 1, b = 2))
    })

    test_that("a named list can be converted to a container",
    {
        nl <- list(a = 1, b = 2)
        ee(as.container(nl), container(a = 1, b = 2))
    })

    test_that("a named vector with empty names can be converted to a container",
    {
        nv <- c(a = 1, b = 2, "")
        ee(as.container(nv), container(a = 1, b = 2, ""))
    })

    test_that("a named list with empty names can be converted to a container",
    {
        nl <- list(a = 1, b = 2, "")
        ee(as.container(nl), container(a = 1, b = 2, ""))
    })

    test_that("a named list with NULL values can be converted to a container",
    {
        nl <- list(a = 1, b = NULL, c = 2)
        ee(as.container(nl), container(a = 1, b = NULL, c = 2))
    })

    test_that("a named list with empty values can be converted to a container",
    {
        nl <- list(a = 1, b = numeric(0), c = 2)
        ee(as.container(nl), container(a = 1, b = numeric(0), c = 2))
    })
})


describe(
    "as.cont",
{
    ee <- expect_equal

    test_that("as.cont is a shortcut for as.container",
    {
        ee(as.cont(numeric()), as.container(numeric()))
        ee(as.cont(NULL), as.container(NULL))
        ee(as.cont(list()), as.container(list()))
        ee(as.cont(1), as.container(1))
        ee(as.cont(1:2), as.container(1:2))
        ee(as.cont(container(1)), as.container(container(1)))
    })
})


describe("is.container",
{
    test_that("is.container works as expected",
    {
        expect_false(is.container(0))
        expect_false(is.container(list()))

        expect_true(is.container(container()))
        expect_true(is.container(container(NULL)))
        expect_true(is.container(setnew()))
        expect_true(is.container(dict()))
        expect_true(is.container(deque()))
    })

    test_that("signals empty arg",
    {
        expect_error(
            is.container(),
            "argument \"x\" is missing, with no default"
        )
    })
})


describe("as.list.Container",
{
    ee <- expect_equal

    test_that("container objects can be converted to lists",
    {
        ee(as.list(cont()), list())
        ee(as.list(cont(NULL)), list(NULL))
        ee(as.list(cont(1)), list(1))
        ee(as.list(cont(1, 2, 3)), list(1, 2, 3))
        ee(as.list(cont(a = 1, b = 2)), list(a = 1, b = 2))
        ee(as.list(cont(a = 1, b = NULL)), list(a = 1, b = NULL))
        ee(as.list(cont(a = 1, b = numeric(0))), list(a = 1, b = numeric(0)))
        ee(as.list(cont(a = 1, b = list())), list(a = 1, b = list()))
        ee(as.list(cont(a = 1, b = cont(2))), list(a = 1, b = cont(2)))
        ee(as.list(cont(a = 1, b = cont(2, 3))), list(a = 1, b = cont(2, 3)))
    })

    test_that("nested containers are converted as copies by default",
    {
        c1 <- cont(1)
        cc1 <- Container$new(c1)
        ccc1 <- Container$new(cc1)

        l <- as.list(ccc1)
        ee(l, list(cont(cont(1))))

        c1$add(2)
        cc1$add(2)

        expect_true(all.equal(ccc1, cont(cont(cont(1, 2), 2))))
        # Containers in list were copied and thus are not changed
        expect_true(all.equal(l, list(cont(cont(1)))))
    })

    test_that("nested containers can be converted as shallow copies",
    {
        c1 <- cont(1)
        cc1 <- Container$new(c1)
        ccc1 <- Container$new(cc1)

        l <- as.list(ccc1, deep = FALSE)
        ee(l, list(cont(cont(1))))

        c1$add(2)
        cc1$add(2)

        # Containers in list are updated
        expect_true(all.equal(l, list(cont(cont(1, 2), 2))))
    })
})


describe("c.Container",
{
    ee <- expect_equal

    test_that("standard non-recursive concatenation works",
    {
        ee(as.list(c(cont())), c(list()))
        ee(as.list(c(cont(1))), c(list(1)))
        ee(as.list(c(cont(NULL))), c(list(NULL)))

        ee(as.list(c(cont(), cont())), c(list(), list()))
        ee(as.list(c(cont(1), cont())), c(list(1), list()))
        ee(as.list(c(cont(1), cont(2))), c(list(1), list(2)))
        ee(
            as.list(c(cont(1), cont(2, list(a = 3)))),
            c(list(1), list(2, list(a = 3)))
        )
        ee(
            as.list(c(cont(1), cont(2, cont(a = 3)))),
            c(list(1), list(2, cont(a = 3)))
        )
        ee(
            c(cont(1), cont(a = 2, b = cont(a = 3))),
            cont(1, a = 2, b = cont(a = 3))
        )
    })

    test_that("use.names = FALSE is consistent with base R lists",
    {
        ee(
            c(cont(a = 1), cont(a = 2, c = cont(a = 3)), use.names = FALSE),
            cont(1, 2, cont(a = 3))
        )

        ee(
            c(list(a = 1), list(a = 2, c = list(a = 3)), use.names = FALSE),
            list(1, 2, list(a = 3))
        )
    })

    test_that("recursive concatenation is consistent with base R lists",
    {
        cr <- function(...) c(..., recursive = TRUE)

        ee(cr(cont()), cr(list()))
        ee(cr(cont(1)), cr(list(1)))
        ee(cr(cont(NULL)), cr(list(NULL)))

        ee(
            cr(cont(), cont()),
            cr(list(), list())
        )
        ee(
            cr(cont(1), cont()),
            cr(list(1), list())
        )
        ee(
            cr(cont(1), cont(2)),
            cr(list(1), list(2))
        )
        ee(
            cr(cont(1), cont(2, 3)),
            cr(list(1), list(2, 3))
        )
        ee(
            cr(cont(1), cont(2, list(a = 3))),
            cr(list(1), list(2, list(a = 3)))
        )
        ee(
            cr(cont(1), cont(2, cont(a = 3))),
            cr(list(1), list(2, list(a = 3)))
        )
        ee(
            cr(cont(1), list(2, cont(a = 3))),
            cr(list(1), list(2, list(a = 3)))
        )
        ee(
            cr(cont(1), list(2, cont(a = 3))),
            cr(list(1), list(2, list(a = 3)))
        )
        ee(
            cr(cont(), list(2, cont(a = 3))),
            cr(list(), list(2, list(a = 3)))
        )

        ee(
            cr(cont(1), cont(a = 2, b = cont(a = 3))),
            c(1, a = 2, b.a = 3)
        )
    })

    test_that("ensure concatenated objects are always copies",
    {
        c1 <- cont(1)
        c2 <- cont(2)
        c1c1 <- cont(c1 = c1)

        cc <- c(c1, c1c1, c2)
        ee(unpack(cc), c(1, c1 = 1, 2))
        c1$add(2)
        ee(unpack(cc), c(1, c1 = 1, 2)) # still the same

    })
})


describe("length.Container",
{
    ee <- expect_equal

    test_that("length returns the number of elements in a Container",
    {
        ee(length(cont()), 0)
        ee(length(cont(1)), 1)
        ee(length(cont(numeric())), 1)
        ee(length(cont(NULL)), 1)
        ee(length(cont(1, as.cont(1:10))), 2)
    })

    test_that("length works with nested containers",
    {
        co <- cont(1, cont(2, 3), cont(a = 4, b = 5))
        ee(length(co), 3)
        ee(length(co[[2]]), 2)
        ee(length(co[[3]]), 2)
        ee(length(co[[3]][[1]]), 1)
    })
})


describe(
    "names.Container",
{
    ee <- expect_equal

    test_that("returns NULL for empty containers",
    {
        ee(names(cont()), NULL)
        ee(names(cont(numeric())), NULL)
        ee(names(cont(list())), NULL)
        ee(names(cont(1, 2, 3)), NULL)
    })
    test_that("returns the expected names for named containers",
    {
        ee(names(cont(a = 1, 2, x = 5)), c("a", "", "x"))
    })
})


describe(
    "str.Container",
{
    test_that("structure of a Container is printed as expected",
    {
        co <- cont(1:3, cont("a", 1))
        out <- utils::capture.output(str(co))

        expected_out <- c(
            "Container of 2 ",
            " $ : int [1:3] 1 2 3",
            " $ :Container of 2 ",
            "  ..$ : chr \"a\"",
            "  ..$ : num 1"
        )

        expect_equal(out, expected_out)
    })
})


describe(
    "names<-.Container",
{
    ee <- expect_equal

    test_that("names can be set for containers",
    {
        co <- cont(a = 1, b = 2, c = 3)
        names(co) <- LETTERS[1:3]
        ee(names(co), LETTERS[1:3])

        names(co)[3] <- "z"
        ee(names(co), c("A", "B", "z"))

        names(co)[1:2] <- c("x", "y")
        ee(names(co), c("x", "y", "z"))
    })

    test_that("new names can be set on initially unnamed containers",
    {
        co <- cont(1, 2, 3)
        names(co) <- letters[1:3]
        ee(names(co), letters[1:3])
    })

    test_that("fully named containers can be unnamed",
    {
        co <- cont(a = 1, b = 2, c = 3)
        names(co) <- NULL
        ee(names(co), NULL)
    })

    test_that("setting new names that would lead to duplication is prevented",
    {
        co <- cont(a = 1, b = 2, c = 3)
        expect_error(names(co)[2] <- "a", "has duplicated names: 'a'")
        ee(names(co), c("a", "b", "c"))
        expect_error(names(co) <- c("a", "b", "b"), "has duplicated names: 'b'")
        ee(names(co), c("a", "b", "c"))
    })

    test_that("setting names partially is prevented if names of container
        already are not uniquely distinguishable",
    {
        co <- cont(a = 1, 2, 3)
        expect_error(names(co)[2] <- "b", "'old' has duplicated names: ''")
    })
})


describe("%in% with Container",
{
    co <- container(a = 1, 2, b = 3)
    li <- as.list(co)

    describe("Container on RHS",
    {
        test_that("unnamed vectors match with base R",
        {
            expect_equal(1 %in% container(), 1 %in% list())
            expect_equal(1 %in% co, 1 %in% li)
            expect_equal(4 %in% co, 4 %in% li)
            v <- c(1, 3, 5, 2)
            expect_equal(v %in% co, v %in% li)
            expect_equal("a" %in% co, "a" %in% li)
        })

        test_that("named vectors keep names in contrast to base R",
        {
            v <- c(x = 1, y = 2, z = 9)
            expect_equal(v %in% co, c(x = TRUE, y = TRUE, z = FALSE))
            expect_equal(unname(v %in% co), v %in% li)
        })

        test_that(
            "unnamed list elements match with base R",
        {
            l <- list(1, 2, 9)
            expect_equal(l %in% co, c(TRUE, TRUE, FALSE))
            expect_equal(l %in% co, l %in% li)

            expect_equal(
                list(x = 1, y = 2, z = 9) %in% co,
                c(x = TRUE, y = TRUE, z = FALSE)
            )
            expect_equal(
                unname(list(x = 1, y = 2, z = 9) %in% co),
                list(x = 1, y = 2, z = 9) %in% lapply(li, as.character)
            )
        })

        test_that("named lists keep names in contrast to base R",
        {
            l <- list(x = 1, y = 2, z = 9)
            expect_equal(l %in% co, c(x = TRUE, y = TRUE, z = FALSE))
            expect_equal(unname(l %in% co), l %in% li)
        })

        test_that("NA handling matches base R for unnamed vectors",
        {
            co2 <- container(a = NA_real_, b = 2)
            li2 <- as.list(co2)

            expect_true(NA_real_ %in% co2)
            expect_false(NA_real_ %in% li2)
            expect_equal(NA_real_ %in% co2, NA_real_ %in% unlist(li2))

            v <- c(1, NA_real_, 2)
            expect_equal(v %in% co2, c(FALSE, TRUE, TRUE))
            expect_equal(v %in% li2, c(FALSE, FALSE, TRUE))
            expect_equal(v %in% co2, v %in% unlist(li2))

            v <- c(x = 1, y = NA_real_, z = 2)
            expect_equal(v %in% co2, c(x = FALSE, y = TRUE, z = TRUE))
            expect_equal(v %in% li2, c(FALSE, FALSE, TRUE))
            expect_equal(unname(v %in% co2), v %in% unlist(li2))
        })
    })

    describe("Container on LHS",
    {
        test_that("standard cases",
        {
            expect_equal(co %in% c(), c("a" = FALSE, FALSE, "b" = FALSE))
            expect_equal(co %in% 1:4, c("a" = TRUE, TRUE, "b" = TRUE))
            expect_equal(co %in% 2:5, c("a" = FALSE, TRUE, "b" = TRUE))
        })
    })


    describe("Container vs Container",
    {
        test_that("standard cases",
        {
            expect_equal(co %in% co, c("a" = TRUE, TRUE, "b" = TRUE))
            expect_equal(
                co %in% container(a = 2, b = 1),
                c("a" = TRUE, TRUE, "b" = FALSE)
            )
            expect_equal(
                co %in% container(a = 5, b = 6),
                c("a" = FALSE, FALSE, "b" = FALSE)
            )
        })

        test_that("Deep equality for nested container elements",
        {
            coli <- container(a = list(x = 1), b = list(x = 2))
            coco <- container(a = container(x = 1), b = container(x = 2))

            expect_true(1 %in% coli)
            expect_false(1 %in% coco)

            expect_true(list(x = 1) %in% coli)
            expect_false(list(x = 1) %in% coco)

            expect_true(container(list(x = 1)) %in% coli)
            expect_false(container(list(x = 1)) %in% coco)

            expect_true(container(container(x = 1)) %in% coco)
            expect_equal(coco %in% coco, c("a" = TRUE, "b" = TRUE))
        })
    })
})
