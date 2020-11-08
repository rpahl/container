context("dict.frame S3")

test_that("[[.Dict.frame operator extracts values as expected", {
    df = data.frame(A = 1:3, B = 4:6)
    dif = dict.frame(list(A = 1:3, B = 4:6))
    expect_equal(dif[[1, 2]], df[[1, 2]])
    expect_equal(dif[[2, "A"]], df[[2, "A"]])
    expect_equal(dif[[1]], df[[1]])
    expect_equal(dif[["A"]], df[["A"]])
    expect_error(dif[[3]], "subscript out of bounds")
    expect_error(df[[3]], "subscript out of bounds")

    expect_equal(dif[["C"]], df[["C"]])

    expect_error(dif[["C", default = 0]],
                 "length(default) == nrow(x) is not TRUE", fixed = TRUE)

    expect_equal(dif[["C", default = 7:9]], 7:9)
    expect_equal(dif[[1, "C", default = 7:9]], 7)
    expect_equal(dif[[1, 3, default = 7:9]], 7)

    expect_error(dif[[4, "A"]], "subscript out of bounds")
    expect_error(df[[4, "A"]], "subscript out of bounds")
})


test_that("[.Dict.frame operator extracts values as expected", {
    df = data.frame(A = 1:3, B = 4:6, C = 7:9)
    dif = dict.frame(df)
    expect_equal(as.data.frame(dif[, 1]), df[, 1, drop = FALSE])
    expect_equal(as.data.frame(dif[, "A"]), df[, "A", drop = FALSE])

    expect_equal(as.data.frame(dif[1:2, ]), df[1:2, ])
    expect_equal(as.data.frame(dif[c(1, 3), c("A", "B")]),
                 df[c(1, 3), c("A", "B")])

    df.expected = df[c(3, 7), 2, drop = FALSE]
    attr(df.expected, "row.names") = as.integer(c(3, 7))
    expect_equal(df.expected, as.data.frame(dif[c(3, 7), 2]))

    # Inconsistent data.frame behaviour
    expect_equal(df[1:2, 4], NULL)
    expect_error(df[, 4], "undefined columns selected")

    expect_error(dif[1:2, 4],                "4 - subscript out of bounds")
    expect_error(dif[1:2, 4, default = 7:9], "4 - subscript out of bounds")
    expect_error(dif[ , 4],                  "4 - subscript out of bounds")
    expect_error(dif[ , 4, default = 7:9],   "4 - subscript out of bounds")

    expect_equal(as.data.frame(dif[1:2, c("B", "X"), default = 7:9]),
                 data.frame(B = 4:5, X = 7:8))

    expect_equal(df[1, "X"], NULL)
    expect_equal(as.data.frame(dif[1, "X"]), data.frame())
    expect_equal(as.data.frame(dif[1:2, "X"]), data.frame())
    expect_equal(as.data.frame(dif[1:2, c("X", "Y")]), data.frame())
})


test_that("[.Dict.frame operator can be used with list subscript", {
    df = data.frame(A = 1:3, B = 4:6, C = 7:9)
    dif = dict.frame(df)
    expect_equal(dif[, list(1, "B")], dif[, 1:2])
    expect_equal(dif[c(1, 3), list(1, "B")], dif[c(1, 3), 1:2])
    expect_equal(dif[1, list("B", 1)], dif[1, 2:1])

    expect_error(dif[, list(1, "A")], "key 'A' already in Dict.frame")
})

