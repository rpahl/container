co = container_options

# With no arg all options should be returned
expect_equal(co(), .default_options())

# Retrieve options one by one
for (name in names(.default_options()))
    expect_equal(co(name), .default_options()[[name]])

# Several at once
expect_equal(co("cmp", "vec.len"), unlist(.default_options()[c("cmp", "vec.len")]))
expect_equal(co("cmp", "foo"), unlist(.default_options()["cmp"]))

co(foo = "bar")
expect_equal(co("foo"), c(foo = "bar"))

expect_equal(co(), c(.default_options(), list(foo = "bar")))
co(foo = NULL)
expect_equal(co(), .default_options())

# Several options at once
expect_equal(co(cmp = "all.equal", first.few = FALSE),
             list(cmp = "all.equal", first.few = FALSE))
expect_equal(co(),
             replace(.default_options(), c("cmp", "first.few"), list("all.equal", FALSE)))
