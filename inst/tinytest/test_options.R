co = container_options

# With no arg all options should be returned
expect_equal(co(), .default_options())

# Retrieve options one by one
for (name in names(.default_options()))
    expect_equal(co(name), .default_options()[[name]])

# Several at once
expect_equal(co("cmp", "vec.len"), .default_options()[c("cmp", "vec.len")])
expect_equal(co("cmp", "foo"), .default_options()["cmp"])

co("zzz" = "foo")
expect_equal(co("zzz"), list(zzz = "foo"))

expect_equal(co(), c(.default_options(), list(zzz = "foo")))
co(zzz = NULL)
expect_equal(co(), .default_options())

# Set several options at once
old = co(cmp = "identical", dots = FALSE)
expect_equal(old, .default_options())

expect_equal(co(), replace(.default_options(),
                           c("cmp", "dots"),
                           list("identical", FALSE)))

co(.reset = TRUE)
expect_equal(co(), .default_options())

# Verify that options are sorted
container_options("aaa" = 1, "zzz" = 2)
expect_equal(co(), c(list(aaa = 1), .default_options(), list(zzz = 2)))

