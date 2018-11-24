context("container S3 methods")

test_that("General", {
    expect_true(is.container(container()))
    expect_equal(container(), Container$new())
    expect_equal(as.vector(container()), list())
    ints <- container(1:3)
    expect_output(print(ints),
                  "<Container> of 3 elements:  int \\[1:3\\] 1 2 3")
    expect_equal(as.vector(ints), as.integer(1:3))
    expect_equal(as.list(ints), as.list(1:3))
    ints2 <- ints + ints
    expect_equal(ints$size(), 3)
    expect_equal(ints2$values(), c(ints$values(), ints$values()))
})

test_that("Member functions", {
    co <- container(1:3)
    add(co, 4)
    expect_equal(co$values(), 1:4)
    expect_true(has(co, 4))
    expect_false(has(co, 5))
    discard(co, 4)
    expect_false(has(co, 4))
    expect_equal(discard(co, 4)$values(), 1:3)

    expect_error(remove(co, 4), "4 not in Container")
    remove(co, 1)
    expect_equal(co$values(), 2:3)
    expect_equal(add(co, 2)$values(), c(2, 3, 2))
    discard(co, 2)
    expect_equal(co$values(), 3:2)
    expect_equal(add(co, 3)$values(), c(3, 2, 3))
    discard(co, 3, right=TRUE)
    expect_equal(co$values(), 3:2)
})
