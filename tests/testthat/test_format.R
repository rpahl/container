describe("format", {
    ee <- expect_equal

    describe("format values", {
        test_that("format_values helper works correctly", {
            f <- container:::.format_values

            expect_error(f())
            ee(f(NA), "(NA)")
            ee(f(list(NULL)), "(NULL)")
            ee(f(list(mean)), "(<<function>>)")

            ee(f(as.list(1:10)), "(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L)")
            ee(f(list(a = 1, b = 1:10)), "(a = 1, b = (1L 2L 3L 4L ...))")
            ee(f(list(b = 1:10, m = matrix(1:6, nrow = 2))),
               '(b = (1L 2L 3L 4L ...), m = <<matrix(2x3)>>)')
            ee(f(list(b = 1:10, m = matrix(1:6, nrow = 2))),
               '(b = (1L 2L 3L 4L ...), m = <<matrix(2x3)>>)')
        })
    })

    describe("format.Container", {
        test_that("format works for Container objects", {
            f <- container:::format.Container
            ee(f(container()), "[]")
            ee(f(container(NULL)), "[NULL]")
            ee(f(container(integer())), "[integer()]")
            ee(f(container(numeric())), "[numeric()]")
            ee(f(container(1, b = 2)), "[1, b = 2]")
            ee(f(container(list(a = 1, x = 1:2))), "[list(a = 1, x = (1L 2L))]")
            ee(f(container(list(x = 1:40))), "[list(x = (1L 2L 3L 4L ...))]")

            co <- container(1, 2)
            ee(f(container(co, list(x = co, 3))), '[[1, 2], list(x = [1, 2], 3)]')
            ee(f(container(co, s = setnew(co), 3)), '[[1, 2], s = {[1, 2]}, 3]')
        })
    })

    describe("format.Dict", {
        test_that("format works for Dict objects", {
            f <- container:::format.Dict
            ee(f(dict(a = 1, b = 2:3, c = container(), d = deque(4, 1))),
               "{a = 1, b = (2L 3L), c = Container(), d = |4, 1|}")
        })
    })

    describe("format.Deque", {
        test_that("format works for Deque objects", {
            f <- container:::format.Deque
            ee(f(deque(2, 3, 1)), "|2, 3, 1|")
        })
    })

    describe("format.Set", {
        test_that("format works for Set objects", {
            f <- container:::format.Set
            ee(f(setnew(2, 3, 1)), "{2, 3, 1}")
        })
    })
})
