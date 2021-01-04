# Iterator constructor works as expected
expect_error(iter())
expect_equal(iter(1:3)$length(), 3)
expect_equal(iter(new.env())$length(), 0)
expect_equal(iter(NULL)$length(), 0)
expect_equal(iter(factor(1:2))$length(), 2)
expect_equal(iter(list("a", mean))$length(), 2)
expect_equal(iter(Container$new())$length(), 0)
expect_equal(iter(Container$new(1, 2, 3))$length(), 3)
expect_equal(iter(new.env())$length(), 0)
expect_equal(iter(factor(letters[1:2]))$length(), 2)

# the position of the iterator can be accessed
x <- 1:5
it <- iter(x)
expect_equal(pos(it), 0)
for (i in x) {
    expect_equal(get_next(it), i)
    expect_equal(pos(it), i)
}

# iterator can be moved to being of sequence
it <- iter(1:3)
expect_equal(pos(it), 0)
begin(it)
expect_equal(pos(it), 1)
expect_equal(get_value(it), 1)

# for empty sequence, begin points to 0
expect_equal(pos(begin(iter(NULL))), 0)

# it can be checked if Iterator has next element
expect_true(has_next(iter(1:5)))
expect_false(has_next(iter(list())))

# the value behind the iterator can be retrieved
it <- iter(1:5)
expect_error(get_value(it), "iterator does not point at a value")
next_iter(it)
expect_equal(get_value(it), 1)
next_iter(it)
expect_equal(get_value(it), 2)

# the next value can be retrieved while incrementing the iterator
it <- iter(1)
expect_true(has_next(it))
expect_equal(get_next(it), 1)
expect_false(has_next(it))

# Iterator can be reset
it <- iter(1:3)
expect_equal(pos(it), 0)
while (has_next(it)) next_iter(it)
expect_equal(pos(it), 3)
expect_equal(pos(reset_iter(it)), 0)

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

