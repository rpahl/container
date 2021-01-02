# Iterator constructor works as expected
expect_error(iter(environment()), "'x' is not iterable")
it <- iter(as.list(environment())) # ok
expect_true(is.iterator(it))

# it can be checked if Iterator has next element
expect_true(has_next(iter(1:5)))
expect_false(has_next(iter(list())))

# the value behind the iterator can be retrieved
it <- iter(1:5)
expect_error(get_value(it))
next_iter(it)
expect_equal(get_value(it), 1)
next_iter(it)
expect_equal(get_value(it), 2)

# the next value can be retrieved while incrementing the iterator
it <- iter(1)
expect_true(has_next(it))
expect_equal(get_next(it), 1)
expect_false(has_next(it))

# the position of the iterator can be accessed
x <- 1:5
it <- iter(x)
expect_equal(pos(it), 0)
for (i in x) {
    expect_equal(get_next(it), i)
    expect_equal(pos(it), i)
}

# Iterator can be reset
it <- iter(1:3)
expect_equal(pos(it), 0)
expect_equal(get_next(it), 1)
expect_equal(pos(it), 1)
begin(it)
expect_equal(pos(it), 0)

# Iterator can be incremented
x <- 1:5
it <- iter(x)
for (i in x) {
    expect_equal(pos(next_iter(it)), i)
}

# Iterator works as expected
s <- "Hello World!"
s.split <- strsplit(s, split="")[[1]]
it <- iter(s.split)
s2 <- ""
while(has_next(it)) s2 <- paste0(s2, get_next(it))
expect_equal(s2, s)
expect_false(has_next(it))
expect_error(get_next(it))

