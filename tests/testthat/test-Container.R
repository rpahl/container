context("Container")

test_that("Container", {
    # Initialization and chaining
    expect_true(Container$new()$empty())
    expect_equal(Container$new()$size(), 0)

    # add, has, empty, size
    c1 <- Container$new()
    expect_equal(attr(c1, "name"), "<Container>")
    expect_equal(c1$type(), "list")
    expect_true(c1$empty())
    expect_equal(c1$size(), 0)
    expect_false(c1$has(NULL))

    c1$add(1)
    expect_true(c1$has(1))
    expect_false(c1$empty())
    expect_equal(c1$size(), 1)
    c1$add(1)
    expect_equal(c1$size(), 2)

    # discard and remove
    expect_error(c1$discard(), '"elem" is missing, with no default')
    expect_error(c1$remove(), '"elem" is missing, with no default')
    c1$add(2)
    expect_equal(c1$size(), 3)
    c1$discard(2)
    expect_equal(c1$size(), 2)
    expect_false(c1$has(2))
    expect_equal(c1, c1$discard(2))
    expect_error(c1$remove(2), "2 not in Container")
    expect_equal(c1$size(), 2)
    c1$add(2)$add(1)
    expect_equal(c1$values(), list(1, 1, 2, 1))
    c1$discard(1, right=TRUE)
    expect_equal(c1$values(), list(1, 1, 2))
    expect_true(c1$clear()$empty())

    # Copy semantics
    c1 <- Container$new(1:10)
    expect_equal(c1$type(), "integer")
    c2 <- c1
    cc <- c1$clone()
    expect_true(identical(c1, c2))
    expect_false(identical(c1, cc))
    expect_equal(c1$size(), cc$size())
    expect_equal(c1, cc)

    c1$remove(7)
    expect_true(identical(c1, c2))
    expect_lt(c1$size(), cc$size())

    # Pass list as initialize arg and check against values func
    ll <- list(1, 2, 3, "A", 1:3)
    cc <- Container$new(ll)
    expect_equal(ll, cc$values())

    # Non-trivial objects
    v <- 1:10
    env <- new.env
    ll <- list(1, 2, "A")
    collection <- c(list(v), list(env), list(ll))

    c1 <- Container$new()
    c1$add(v)$add(env)$add(ll)
    expect_equal(c1$values(), collection)
    expect_equal(c1$size(), length(collection))

    # Basic types, type safety and vectorized adding
    ints <- Container$new(integer(0))
    expect_equal(ints$type(), "integer")
    expect_equal(ints$add(1)$type(), "integer")
    expect_equal(ints$add(2.3)$type(), "integer")
    expect_equal(ints$values(), as.integer(1:2))
    expect_warning(ints$add("a"), "NAs introduced by coercion")
    expect_equal(Container$new(0L)$add(1:3)$values(), as.integer(0:3))

    # Other types
    expect_error(Container$new(new.env()),
                 "cannot coerce type 'environment' to vector of type 'any'")
    expect_equal(Container$new(TRUE)$type(), "logical")
    expect_error(Container$new(function(){}),
                 "cannot coerce type 'closure' to vector of type 'any'")
    expect_equal(Container$new(raw())$type(), "raw")
    expect_equal(Container$new(0+0i)$type(), "complex")
    expect_equal(Container$new(letters[1:10])$type(), "character")

    # Adding other containers
    v <- 1:10
    co <- Container$new(v)
    co$add(co)
    expect_equal(co$values(), rep(v, 2))

    # Apply
    co <- Container$new(as.numeric(v))
    expect_equal(unlist(co$apply(f = base::log)), log(v))
})

