# -----------------
# container_options
# -----------------

defaults = container:::.default_options
co = container_options
co(.reset = TRUE)


# With no arg all options should be returned
expect_equal(co(), defaults())

# Several at once
expect_equal(co("compare", "vec.len"),
             defaults()[c("compare", "vec.len")])
expect_equal(co("compare", "foo"), defaults()["compare"])

co("zzz" = "foo")
expect_equal(co("zzz"), list(zzz = "foo"))

expect_equal(co(), c(defaults(), list(zzz = "foo")))
co(zzz = NULL)
expect_equal(co(), defaults())

# Set several options at once
old = co(compare = "identical", useDots = FALSE)
expect_equal(old, defaults())

expect_equal(co(), replace(defaults(),
                           c("compare", "useDots"),
                           list("identical", FALSE)))

co(.reset = TRUE)
expect_equal(co(), defaults())

# Verify that options are sorted
container_options(".aa" = 1, "zzz" = 2)
expect_equal(co(), c(list(.aa = 1), defaults(), list(zzz = 2)))


# ------------------
# getContainerOption
# ------------------
expect_equal(getContainerOption("useDots"), container_options()[["useDots"]])
expect_equal(getContainerOption("compare"), container_options()[["compare"]])

expect_equal(getContainerOption("bla"), NULL)
expect_equal(getContainerOption("bla", default = 1), 1)

expect_error(getContainerOption(c("useDots", "compare")),
             "'x' must be a character string")

