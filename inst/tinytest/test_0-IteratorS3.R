# is.iterable
expect_true(is.iterable(container()))


# iter initialization works as expected
expect_error(iter())
expect_error(iter(list()), "must be iterable or subsettable")
e = new.env()
e$a = 1
expect_error(iter(e), "must be iterable or subsettable")

expect_equal(length(iter(list(), .subset = .subset)), 0)
expect_equal(iter(1:3)$length(), 3)
expect_equal(iter(factor(1:2))$length(), 2)
expect_equal(iter(list("a", mean))$length(), 2)
expect_equal(iter(Container$new())$length(), 0)
expect_equal(iter(Container$new(1, 2, 3))$length(), 3)
expect_equal(iter(factor(letters[1:2]))$length(), 2)


# the position of the iterator can be accessed
x <- 1:5
it <- iter(x)
expect_equal(pos(it), 0)
for (i in x) {
    expect_equal(get_next(it), i)
    expect_equal(pos(it), i)
}

# iterator can be moved to begin of the sequence
it <- iter(1:3)
expect_equal(pos(it), 0)
begin(it)
expect_equal(pos(it), 1)
expect_equal(get_value(it), 1)

# it can be checked if Iterator has next element
expect_true(has_next(iter(1:5)))
expect_false(has_next(next_iter(iter(1))))

# the value behind the iterator can be retrieved if iterator points at one
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

while (has_next(it))
    next_iter(it)

expect_equal(pos(it), 3)
expect_equal(pos(reset_iter(it)), 0)


# Iterator works as expected
s <- "Hello World!"
s.split <- strsplit(s, split="")[[1]]
it <- iter(s.split)
s2 <- ""
while(has_next(it))
    s2 <- paste0(s2, get_next(it))

expect_equal(s2, s)
expect_false(has_next(it))
expect_error(get_next(it))

# Iterator works by value (i.e. on a copy) when used with a Container object
co = container(1, 2, 3)
it = iter(co)
expect_equal(length(co), length(it))
next_iter(it)

expect_equal(get_value(it), list(1))
co$discard(1)
expect_equal(get_value(it), list(1))
expect_equal(length(co), length(it) - 1)

co$clear()
expect_equal(get_value(it), list(1))
expect_equal(length(it), 3)


# Iterator works by value (i.e. on a copy) when used with a Dict object
d = dict(a = 1, b = 2)
it = iter(d)
expect_equal(length(d), length(it))

next_iter(it)
expect_equal(get_value(it), list(a = 1))
d$discard("a")
expect_equal(get_value(it), list(a = 1))

# Iterator works by value (i.e. on a copy) when used with a Set object
s = setnew(a = 1, 2, 3)
it = iter(s)

next_iter(it)
expect_equal(get_value(it), list(a = 1))
s$discard(1)
expect_equal(get_value(it), list(a = 1))

