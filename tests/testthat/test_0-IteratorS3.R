
describe("iter",
{
    test_that("Iterator constructor works as expected",
    {
        it <- iter(NULL)
        expect_equal(length(it), 0)
        expect_false(has_next(it))

        it <- iter(list(), .subset = .subset)
        expect_equal(length(it), 0)
        expect_false(has_next(it))

        expect_equal(length(iter(1:3)), 3)

        expect_equal(length(iter(factor(1:2))), 2)
        expect_equal(length(iter(factor(letters[1:2]))), 2)

        expect_equal(length(iter(list("a", mean))), 2)

        expect_equal(length(iter(container(1, 2, 3))), 3)
    })

    test_that("works an empty Container as it derives from Iterable",
    {
        expect_equal(length(iter(container())), 0)
    })

    test_that("Iterator constructor signals missing arg",
    {
        expect_error(iter(), 'argument "x" is missing')
    })

    it("signals if passed object is not subsettable",
    {
        expect_error(iter(list()), "x must be iterable or subsettable")
        e <- new.env()
        e$a <- 1
        expect_error(iter(e), "must be iterable or subsettable")
    })
})


describe("is.iterator",
{
    test_that("is.iterator works as expected",
    {
        expect_true(is.iterator(iter(1:5)))
        expect_false(is.iterator(1:5))
        expect_false(is.iterator(NULL))
    })
})


describe("is.iterable",
{
    test_that("detects Iterable objects",
    {
        expect_true(is.iterable(container(1, 2, 3)))
        expect_true(is.iterable(dict(a = 1, b = 2)))
        expect_true(is.iterable(setnew(a = 1, 2, 3)))

        expect_false(is.iterable(1:5))
        expect_false(is.iterable(NULL))
        expect_false(is.iterable(iter(1:5)))
    })
})


describe("begin",
{
    test_that("iterator can be moved to begin of the sequence",
    {
        it <- iter(1:3)
        expect_equal(pos(it), 0)
        begin(it)
        expect_equal(pos(it), 1)
        expect_equal(get_value(it), 1)

        next_iter(it)
        next_iter(it)
        expect_equal(pos(it), 3)
        expect_equal(get_value(it), 3)
        begin(it)
        expect_equal(pos(it), 1)
        expect_equal(get_value(it), 1)
    })
})


describe("get_value",
{
    test_that(
        "the value behind the iterator can be retrieved if it points at one",
    {
        it <- iter(1:5)
        expect_error(get_value(it), "iterator does not point at a value")
        next_iter(it)
        expect_equal(get_value(it), 1)
        next_iter(it)
        expect_equal(get_value(it), 2)
    })
})


describe("get_next",
{
    test_that(
        "the next value can be retrieved while incrementing the iterator",
    {
        it <- iter(1)
        expect_true(has_next(it))
        expect_equal(get_next(it), 1)
        expect_false(has_next(it))
    })
})


describe("has_next",
{
    test_that("it can be checked if Iterator has next element",
    {
        expect_true(has_next(iter(1:5)))
        expect_false(has_next(next_iter(iter(1))))
    })
})


describe("has_value",
{
    test_that("it can be checked if Iterator points to a value",
    {
        it <- iter(1)
        expect_false(has_value(it))
        expect_true(has_next(it))
        next_iter(it)
        expect_true(has_value(it))
    })
})


describe("length",
{
    test_that("the number of elements to iterate can be retrieved",
    {
        it <- iter(1:5)
        expect_equal(length(it), 5)
        next_iter(it)
        expect_equal(length(it), 5)
    })
})


describe("pos",
{
    test_that("the position of the iterator can be accessed",
    {
        x <- 1:5
        it <- iter(x)
        expect_equal(pos(it), 0)
        for (i in x) {
            expect_equal(get_next(it), i)
            expect_equal(pos(it), i)
        }
    })
})


describe("next_iter",
{
    test_that("the iterator can be incremented",
    {
        it <- iter(1:3)
        expect_equal(pos(it), 0)
        next_iter(it)
        expect_equal(pos(it), 1)
        next_iter(it)
        expect_equal(pos(it), 2)
        next_iter(it)
        expect_equal(pos(it), 3)

        expect_error(next_iter(it), "Iterator has no more elements.")
    })
})


describe("reset_iter",
{
    test_that("the iterator can be reset",
    {
        it <- iter(1:3)
        expect_equal(pos(it), 0)

        next_iter(it)
        next_iter(it)
        expect_equal(pos(it), 2)
        expect_equal(get_value(it), 2)
        reset_iter(it)
        expect_equal(pos(it), 0)
        expect_false(has_value(it))
    })
})
