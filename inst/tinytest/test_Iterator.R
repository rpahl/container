# is.iterable
expect_true(is.iterable(Container$new()))
expect_false(is.iterable(1))

# is.subsettable
f = as.factor(1:3)
expect_false(is.vector(f))
expect_true(is.subsettable(f))
f = formula(y ~ x + 1)
expect_false(is.vector(f))
expect_true(is.subsettable(f))
expect_true(is.subsettable(1))
expect_true(is.subsettable(data.frame(a = 1)))
expect_true(is.subsettable(expression(x + 1)))

expect_false(is.subsettable(NULL))
expect_false(is.subsettable(list()))
expect_false(is.subsettable(numeric()))
expect_false(is.subsettable(data.frame()))
expect_false(is.subsettable(dict.table()))


# Iterator constructor works as expected
expect_error(Iterator$new(environment()), "'x' is not iterable")
it <- Iterator$new(as.list(environment())) # ok

# it can be checked if Iterator has next element
expect_true(Iterator$new(1:5)$has_next())
expect_false(Iterator$new(list())$has_next())

# the value behind the iterator can be retrieved
it <- Iterator$new(1:5)
expect_error(it$get_value())
it$next_iter()
expect_equal(it$get_value(), 1)
it$next_iter()
expect_equal(it$get_value(), 2)

# the next value can be retrieved while incrementing the iterator
it <- Iterator$new(1)
expect_true(it$has_next())
expect_equal(it$get_next(), 1)
expect_false(it$has_next())

# the position of the iterator can be accessed
x <- 1:5
it <- Iterator$new(x)
expect_equal(it$pos(), 0)
for (i in x) {
    expect_equal(it$get_next(), i)
    expect_equal(it$pos(), i)
}

# Iterator can be reset
it <- Iterator$new(1:3)
expect_equal(it$pos(), 0)
expect_equal(it$get_next(), 1)
expect_equal(it$pos(), 1)
it$begin()
expect_equal(it$pos(), 0)

# Iterator can be incremented
x <- 1:5
it <- Iterator$new(x)
for (i in x) {
    expect_equal(it$next_iter()$pos(), i)
}

# Iterator works as expected
s <- "Hello World!"
s.split <- strsplit(s, split="")[[1]]
it <- Iterator$new(s.split)
s2 <- ""
while(it$has_next()) s2 <- paste0(s2, it$get_next())
expect_equal(s2, s)
expect_false(it$has_next())
expect_error(it$get_next())

