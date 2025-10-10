describe("is_empty",
{
    test_that("is_empty.Container works correctly", {
        co <- container(1, 2)
        expect_false(is_empty(co))
        expect_true(is_empty(clear(co)))
    })

    test_that("is_empty.dict.table works correctly", {
        d <- dict.table(a = 1:4, b = 4:1)
        expect_false(is_empty(d))
        expect_true(is_empty(clear(d)))
    })
})
