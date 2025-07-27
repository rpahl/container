
describe(".is.subsettable",
{
    it("works as expected",
    {
        fac <- as.factor(1:3)
        expect_false(is.vector(fac))
        expect_true(.is.subsettable(fac))
        form <- formula(y ~ x + 1)
        expect_false(is.vector(form))
        expect_true(.is.subsettable(form))
        expect_true(.is.subsettable(1))
        expect_true(.is.subsettable(data.frame(a = 1)))
        expect_true(.is.subsettable(expression(x + 1)))

        expect_false(.is.subsettable(list()))
        expect_true(.is.subsettable(list(), .subset = .subset))
    })
})


describe("initialize",
{
    test_that("Iterator constructor works as expected",
    {
        expect_equal(Iterator$new(NULL)$length(), 0)
        expect_false(Iterator$new(NULL)$has_next())

        expect_equal(Iterator$new(list(), .subset = .subset)$length(), 0)
        expect_false(Iterator$new(list(), .subset = .subset)$has_next())

        expect_equal(Iterator$new(1:3)$length(), 3)

        expect_equal(Iterator$new(factor(1:2))$length(), 2)
        expect_equal(Iterator$new(factor(letters[1:2]))$length(), 2)

        expect_equal(Iterator$new(list("a", mean))$length(), 2)

        expect_equal(Iterator$new(Container$new(1, 2, 3))$length(), 3)
    })

    test_that("works an empty Container as it derives from Iterable",
    {
        expect_equal(Iterator$new(Container$new())$length(), 0)
    })

    test_that("Iterator constructor signals missing arg",
    {
        expect_error(Iterator$new(), 'argument "x" is missing')
    })

    it("signals if passed object is not subsettable",
    {
        expect_error(Iterator$new(list()), "x must be iterable or subsettable")
        e <- new.env()
        e$a <- 1
        expect_error(Iterator$new(e), "must be iterable or subsettable")
    })
})


describe("member functions",
{
    test_that("iterator can be moved to begin of the sequence",
    {
        it <- Iterator$new(1:3)
        expect_equal(it$pos(), 0)
        it$begin()
        expect_equal(it$pos(), 1)
        expect_equal(it$get_value(), 1)

        it$next_iter()
        it$next_iter()
        expect_equal(it$pos(), 3)
        expect_equal(it$get_value(), 3)
        it$begin()
        expect_equal(it$pos(), 1)
        expect_equal(it$get_value(), 1)
    })

    test_that(
        "the value behind the iterator can be retrieved if it points at one",
    {
        it <- Iterator$new(1:5)
        expect_error(it$get_value(), "iterator does not point at a value")
        it$next_iter()
        expect_equal(it$get_value(), 1)
        it$next_iter()
        expect_equal(it$get_value(), 2)
    })

    test_that(
        "the next value can be retrieved while incrementing the iterator",
    {
        it <- Iterator$new(1)
        expect_true(it$has_next())
        expect_equal(it$get_next(), 1)
        expect_false(it$has_next())
    })

    test_that("it can be checked if Iterator has next element",
    {
        expect_true(Iterator$new(1:5)$has_next())
        expect_false(Iterator$new(1)$next_iter()$has_next())
    })

    test_that("it can be checked if Iterator points to a value",
    {
        it <- Iterator$new(1)
        expect_false(it$has_value())
        expect_true(it$has_next())
        it$next_iter()
        expect_true(it$has_value())
    })

    test_that("the number of elements to iterate can be retrieved",
    {
        it <- Iterator$new(1:5)
        expect_equal(it$length(), 5)
        it$next_iter()
        expect_equal(it$length(), 5)
    })

    test_that("the position of the iterator can be accessed",
    {
        x <- 1:5
        it <- Iterator$new(x)
        expect_equal(it$pos(), 0)
        for (i in x) {
            expect_equal(it$get_next(), i)
            expect_equal(it$pos(), i)
        }
    })

    test_that("the iterator can be incremented",
    {
        it <- Iterator$new(1:3)
        expect_equal(it$pos(), 0)
        it$next_iter()
        expect_equal(it$pos(), 1)
        it$next_iter()
        expect_equal(it$pos(), 2)
        it$next_iter()
        expect_equal(it$pos(), 3)

        expect_error(it$next_iter(), "Iterator has no more elements.")
    })

    test_that("the iterator can be reset",
    {
        it <- Iterator$new(1:3)
        expect_equal(it$pos(), 0)

        it$next_iter()
        it$next_iter()
        expect_equal(it$pos(), 2)
        expect_equal(it$get_value(), 2)
        it$reset_iter()
        expect_equal(it$pos(), 0)
        expect_false(it$has_value())
    })
})


describe("iterator use cases",
{
    it("can be used to iterate over a character vector",
    {
        s <- "Hello World!"
        s.split <- strsplit(s, split="")[[1]]
        it <- Iterator$new(s.split)
        s2 <- ""
        while(it$has_next()) s2 <- paste0(s2, it$get_next())
        expect_equal(s2, s)
        expect_false(it$has_next())
        expect_error(it$get_next())
    })

    it("works by reference on Container object",
    {
        co <- container(1, 2, 3)
        it <- co$iter()
        it$next_iter()

        expect_equal(it$get_value(), list(1))
        co$discard(1)
        expect_equal(it$get_value(), list(2))
        co$discard(2)
        expect_equal(it$get_value(), list(3))
        co$clear()
        expect_error(it$get_value(), "iterator does not point at a value")
        co$add(4)
        expect_equal(it$get_value(), list(4))
        expect_equal(it$pos(), 1)
    })

    it("works by reference on Dict object",
    {
        d <- dict(a = 1, b = 2)
        it <- d$iter()
        it$next_iter()
        expect_equal(it$get_value(), list(a = 1))
        d$discard_at("a")
        expect_equal(it$get_value(), list(b = 2))
    })

    it("works by reference on Set object",
    {
        s <- setnew(a = 1, 2, 3)
        it <- s$iter()
        it$next_iter()
        expect_equal(it$get_value(), list(a = 1))
        s$discard(1)
        expect_equal(it$get_value(), list(2))
    })
})
