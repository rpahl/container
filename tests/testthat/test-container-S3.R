context("container S3 methods")

test_that("container", {
    # Initialization and chaining
    expect_true(is.container(container()))
    expect_true(empty(container()))
    expect_equal(size(container()), 0)

    # add, has, empty, size
    c1 <- container()
    expect_equal(attr(c1, "name"), "<Container>")
    expect_equal(type(c1), "list")
    expect_true(empty(c1))
    expect_equal(size(c1), 0)
    expect_false(has(c1, NULL))

    add(c1, 1)
    expect_true(has(c1, 1))
    expect_false(empty(c1))
    expect_equal(size(c1), 1)
    add(c1, 1)
    expect_equal(size(c1), 2)

    # discard and remove
    expect_error(discard(c1), '"elem" is missing, with no default')
    expect_error(remove(c1), '"elem" is missing, with no default')
    add(c1, 2)
    expect_equal(size(c1), 3)
    discard(c1, 2)
    expect_equal(size(c1), 2)
    expect_false(has(c1, 2))
    expect_equal(c1, discard(c1, 2))
    expect_error(remove(c1, 2), "2 not in Container")
    expect_equal(size(c1), 2)
    add(add(c1, 2), 1)
    expect_equal(values(c1), list(1, 1, 2, 1))
    discard(c1, 1, right=TRUE)
    expect_equal(values(c1), list(1, 1, 2))
    expect_true(empty(clear(c1)))

    # print
    ints <- container(1:3)
    expect_output(print(ints),
                  "<Container> of 3 elements:  int \\[1:3\\] 1 2 3")

    # Copy semantics
    c1 <- container(1:10)
    expect_equal(type(c1), "integer")
    c2 <- c1
    cc <- clone(c1)
    expect_true(identical(c1, c2))
    expect_false(identical(c1, cc))
    expect_equal(size(c1), size(cc))
    expect_equal(c1, cc)

    remove(c1, 7)
    expect_true(identical(c1, c2))
    expect_lt(size(c1), size(cc))

    # Pass list as initialize arg and check against values func
    ll <- list(1, 2, 3, "A", 1:3)
    cc <- container(ll)
    expect_equal(ll, values(cc))

    # Non-trivial objects
    v <- 1:10
    env <- new.env
    ll <- list(1, 2, "A")
    collection <- c(list(v), list(env), list(ll))

    c1 <- container()
    lapply(collection, add.Container, x=c1)
    expect_equal(values(c1), collection)
    expect_equal(size(c1), length(collection))

    # Basic types, type safety and vectorized adding
    ints <- container(integer(0))
    expect_equal(type(ints), "integer")
    expect_equal(type(add(ints, 1)), "integer")
    expect_equal(type(add(ints, 2.3)), "integer")
    expect_equal(values(ints), as.integer(1:2))
    expect_warning(add(ints, "a"), "NAs introduced by coercion")
    expect_equal(values(add(container(0L), 1:3)), as.integer(0:3))

    # Other types
    expect_error(container(new.env()),
                 "cannot coerce type 'environment' to vector of type 'any'")
    expect_equal(type(container(TRUE)), "logical")
    expect_error(container(function(){}),
                 "cannot coerce type 'closure' to vector of type 'any'")
    expect_equal(type(container(raw())), "raw")
    expect_equal(type(container(0+0i)), "complex")
    expect_equal(type(container(letters[1:10])), "character")

    # Adding other containers
    v <- 1:10
    co <- container(v)
    add(co, co)
    expect_equal(values(co), rep(v, 2))

    # Apply
    co <- container(as.numeric(v))
    expect_equal(unlist(lapply(values(co), FUN=log)), log(v))
})

test_that("Operator", {
    ints <- container(1:3)
    ints2 <- ints + ints
    expect_equal(ints$size(), 3)
    expect_equal(ints2$values(), c(ints$values(), ints$values()))
})

test_that("Conversion", {
    ints <- container(1:3)
    expect_equal(as.vector(container()), list())
    expect_equal(as.vector(ints), as.integer(1:3))
    expect_equal(as.list(ints), as.list(1:3))
})

