context("utils")

test_that("class can be added", {
    v = 1:3
    v = add_class(v, "foo")
    expect_equal(data.class(v), "foo")

    v = add_class(v, "bar")
    expect_equal(data.class(v), "bar")

    v = add_class(v, "foobar", left = FALSE)
    expect_equal(data.class(v), "bar")
})


