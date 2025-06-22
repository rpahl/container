
describe("Deque R6 class",
{
    it("can be initialized with no arguments", {
        d <- Deque$new()
        expect_equal(d$length(), 0L)
        expect_equal(d$values(), list())
    })

    it("can be initialized with a single value", {
        d <- Deque$new(1)
        expect_equal(d$length(), 1L)
        expect_equal(d$values(), list(1))
    })

    it("can be initialized with multiple values", {
        d <- Deque$new(1, 2, 3)
        expect_equal(d$length(), 3L)
        expect_equal(d$values(), list(1, 2, 3))
    })

    it("has the expected class attributes", {
        d <- Deque$new()
        expect_equal(
            attr(d, "class"),
            c("Deque", "Container", "Iterable", "R6")
        )
    })
})


describe("addleft",
{
    it("can add a single element to the left", {
        d <- Deque$new(0L)
        d$addleft(1)
        expect_equal(d$values(), as.list(1:0))
    })
    it("can add a function to the left", {
        d <- Deque$new(mean)
        d$addleft(median)
        expect_equal(d$values(), list(median, mean))
    })
    it("can add multiple named elements to the left", {
        d <- Deque$new(0)
        d$addleft(1, "a")$addleft(2)
        expect_equal(d, Deque$new(2, a = 1, 0))
    })
})


describe("peek",
{
    it("returns the rightmost element", {
        d <- Deque$new(1, 2, 3)
        expect_equal(d$peek(), 3)
    })
    it("returns NULL on empty deques", {
        d <- Deque$new()
        expect_equal(d$peek(), NULL)
    })
})


describe("peekleft",
{
    it("returns the leftmost element", {
        d <- Deque$new(1, 2, 3)
        expect_equal(d$peekleft(), 1)
    })
    it("returns NULL on empty deques", {
        d <- Deque$new()
        expect_equal(d$peekleft(), NULL)
    })
})


describe("pop",
{
    it("returns and removes the rightmost element", {
        d <- Deque$new(1, 2, 3)
        expect_equal(d$pop(), 3)
        expect_equal(d$values(), list(1, 2))
    })

    it("raises an error when popping from an empty deque", {
        d <- Deque$new()
        expect_error(d$pop(), "pop at empty Deque")
    })
})


describe("popleft",
{
    it("returns and removes the leftmost element", {
        d <- Deque$new(1, 2, 3)
        expect_equal(d$popleft(), 1)
        expect_equal(d$values(), list(2, 3))
    })
    it("raises an error when applied to an empty deque", {
        d <- Deque$new()
        expect_error(d$popleft(), "popleft at empty Deque")
    })
})


describe("rev",
{
    it("reverses the elements of the deque", {
        d <- Deque$new(1, 2, 3)
        expect_equal(d$rev()$values(), list(3, 2, 1))
    })

    it("returns an empty deque when called on an empty deque", {
        d <- Deque$new()
        expect_equal(d$rev()$values(), Deque$new()$values())
    })

    it("works with empty or undefined elements", {
        d <- Deque$new(list(), NULL, NA, numeric())
        l <- as.list(d)
        expect_equal(d$length(), 4)
        expect_equal(as.list(d$rev()), rev(l))
    })
})


describe("rotate",
{
    it("rotates the elements to the right", {
        d <- Deque$new(1, 2, 3)
        expect_equal(d$rotate(), Deque$new(3, 1, 2))
    })

    it("rotates the elements to the left", {
        d <- Deque$new(1, 2, 3)
        expect_equal(d$rotate(-1), Deque$new(2, 3, 1))
    })

    it("returns the same deque when rotating by its length", {
        d <- Deque$new(1, 2, 3)
        expect_equal(d$rotate(3), d)
        expect_equal(d$rotate(-3), d)
    })

    it("works with empty or undefined elements", {
        d <- Deque$new(list(), NULL, NA, numeric())
        l <- as.list(d)
        expect_equal(as.list(d$rotate()), l[c(4, 1, 2, 3)])
        expect_equal(as.list(d$rotate(-1)), l)
        expect_equal(as.list(d$rotate(3)), l[c(2, 3, 4, 1)])
    })
})
