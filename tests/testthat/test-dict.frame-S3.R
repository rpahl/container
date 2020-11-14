context("dict.frame S3")

test_that("[[.Dict.frame operator extracts values as expected", {
    df = data.frame(A = 1:3, B = 4:6)
    dif = dict.frame(list(A = 1:3, B = 4:6))
    expect_error(dif[[1, ]], '"j" is missing')
    expect_error(dif[[, 1]], '"i" is missing')

    expect_equal(dif[[1, 2]], df[[1, 2]])
    expect_equal(dif[[2, "A"]], df[[2, "A"]])
    expect_equal(dif[[1]], df[[1]])
    expect_equal(dif[["A"]], df[["A"]])

})

test_that("[[.Dict.frame operator never returns a dict.frame", {
    df = data.frame(A = 1:3, B = 4:6, C = 7:9)
    dif = dict.frame(df)
    expect_false(is.dict.frame(dif[[2]]))
    expect_false(is.dict.frame(dif[[1, 1]]))
})


test_that("[[.Dict.frame operator behaves as expected for out of range indices", {
    df = data.frame(A = 1:2, B = 3:4)
    dif = dict.frame(df)

    # Column undefined index
    expect_equal(dif[["X"]], df[["X"]])
    expect_true(is.null(dif[["X"]]))
    expect_equal(dif[["X", default = 1]], c(1, 1))

    expect_true(is.null(dif[[1, "X"]]))
    expect_true(is.null(df[[1, "X"]]))
    expect_equal(dif[[1, "X", default = 9]], 9)

    # Column out of range
    expect_error(dif[[3]], "3: column index out of bounds")
    expect_error(dif[[3, default = 1]], "3: column index out of bounds")
    expect_error(df[[3]], "subscript out of bounds")

    expect_error(dif[[1, 3]], "3: column index out of bounds")
    expect_error(dif[[1, 3, default = 1]], "3: column index out of bounds")
    expect_error(df[[1, 3]], "subscript out of bounds")

    # Row out of range
    expect_equal(dif[[5, "X"]], df[[5, "X"]])
    expect_true(is.null(dif[[5, "X"]]))
    expect_error(dif[[5, "X", default = 1]], "5: row index out of bounds")

    expect_error(dif[[5, 1]], "5: row index out of bounds")
    expect_error(df[[5, 1]], "subscript out of bounds")
    expect_error(dif[[5, 1, default = 1]], "5: row index out of bounds")

    # Row and column out of range
    expect_error(dif[[3, 4]], "4: column index out of bounds")
    expect_error(dif[[3, 4, default = 9]], "4: column index out of bounds")
})


test_that("[[.Dict.frame operator can provide default values", {
    df = data.frame(A = 1:3, B = 4:6)
    dif = dict.frame(list(A = 1:3, B = 4:6))
    expect_equal(dif[["C", default = 0]], rep(0, nrow(df)))

    expect_error(dif[["C", default = 1:2]], "must be a multiple")

    expect_equal(dif[["C", default = 7:9]], 7:9)
    expect_equal(dif[[1, "C", default = 7:9]], 7)
})


test_that("[.Dict.frame operator returns always a dict.frame", {
    df = data.frame(A = 1:3, B = 4:6, C = 7:9)
    dif = dict.frame(df)

    expect_true(is.dict.frame(dif[, 1]))
    expect_true(is.dict.frame(dif[, "B"]))
    expect_true(is.dict.frame(dif[1, ]))
    expect_true(is.dict.frame(dif[, ]))
})

test_that("[.Dict.frame operator extracts values as expected", {
    df = data.frame(A = 1:3, B = 4:6, C = 7:9)
    dif = dict.frame(df)

    expect_equal(dif[, "B"], dif["B"])
    expect_equal(as.data.frame(dif[, 1]), df[, 1, drop = FALSE])
    expect_equal(as.data.frame(dif[, "A"]), df[, "A", drop = FALSE])

    expect_equal(as.data.frame(dif[1:2, ]), df[1:2, ])
    expect_equal(as.data.frame(dif[c(1, 3), c("A", "B")]),
                 df[c(1, 3), c("A", "B")])

    # dict.frame keeps indices even if undefined
    df.expected = df[c(3, 7), 2, drop = FALSE]
    expect_equal(rownames(df.expected), c("3", "NA"))
    attr(df.expected, "row.names") = as.integer(c(3, 7))
    dif.sub = dif[c(3, 7), 2]
    expect_equal(rownames(dif.sub), c(3, 7))
    expect_equal(df.expected, as.data.frame(dif[c(3, 7), 2]))
})


test_that("[.Dict.frame operator behaves as expected for out of range indices", {
    df = data.frame(A = 1:2, B = 3:4)
    dif = dict.frame(df)

    # Column undefined index
    expect_error(dif["X"], "column 'X' not found")
    expect_error(df["X"], "undefined columns selected")
    expect_equal(dif["X", default = 1], dict.frame(list(X = c(1, 1))))

    expect_error(dif[, "X"], "column 'X' not found")
    expect_error(df[, "X"], "undefined columns selected")
    expect_equal(dif[, "X", default = 1], dict.frame(list(X = c(1, 1))))

    expect_error(dif[c("A", "X")], "column 'X' not found")
    expect_equal(dif[c("A", "X"), default = 9],
                 dict.frame(list(A = 1:2, X = c(9, 9))))

    expect_error(dif[list(1, "X")], "column 'X' not found")
    expect_error(df[c("A", "X")], "undefined columns selected")
    expect_equal(dif[list(1, "X"), default = 9],
                 dict.frame(list(A = 1:2, X = c(9, 9))))

    # Column out of range
    expect_error(dif[3], "3: column index out of bounds")
    expect_error(df[3], "undefined columns selected")
    expect_error(dif[3, default = 1], "3: column index out of bounds")

    expect_error(dif[1, 3], "3: column index out of bounds")
    expect_error(dif[1, 3, default = 9], "3: column index out of bounds")
    expect_error(dif[1:2, 3], "3: column index out of bounds")
    expect_equal(df[1, 3], NULL) # inconsistent data.frame behaviour

    expect_error(dif[, 3], "3: column index out of bounds")
    expect_error(dif[, 3, default = 9], "3: column index out of bounds")
    expect_error(df[, 3], "undefined columns selected")

    expect_error(dif[, 1:3], "3: column index out of bounds")
    expect_error(dif[, 1:3, default = 9], "3: column index out of bounds")
    expect_error(df[, 1:3], "undefined columns selected")

    # Row out of range
    #dif[5, "X"]
    #dif[5, "X", default = 1]
    #expect_equal(df[[5, "X"]])
    #expect_true(is.null(dif[[5, "X"]]))
    #expect_error(dif[[5, 1]], "subscript out of bounds")
    #expect_error(df[[5, 1]], "subscript out of bounds")
})

test_that("[.Dict.frame operator can provide default values", {
    df = data.frame(A = 1:3, B = 4:6, C = 7:9)
    dif = dict.frame(df)

    expect_equal(as.data.frame(dif[1:2, c("B", "X"), default = 0]),
                 data.frame(B = 4:5, X = 0))
    expect_equal(as.data.frame(dif[1:2, c("B", "X"), default = 7:9]),
                 data.frame(B = 4:5, X = 7:8))

    expect_error(dif[1:2, 4, default = 7:9], "4: column index out of bounds")
    expect_error(dif[ , 4, default = 7:9],   "4: column index out of bounds")

    # non-trivial default value
    expect_error(dif[1:2, c("A", "B", "f"), default = base::mean])
    dd = dif[1:2, c("A", "B", "f"), default = list(base::mean)]
    expect_equal(as.list(dd),
                 list(A = 1:2, B = 4:5, f = list(mean, mean)))
})


test_that("[.Dict.frame operator can be used with list subscript", {
    df = data.frame(A = 1:3, B = 4:6, C = 7:9)
    dif = dict.frame(df)
    expect_equal(dif[, list(1, "B")], dif[, 1:2])
    expect_error(df[, list(1, "B")], "invalid subscript type 'list'")
    expect_equal(dif[c(1, 3), list(1, "B")], dif[c(1, 3), 1:2])
    expect_equal(dif[1, list("B", 1)], dif[1, 2:1])

    expect_error(dif[, list(1, "A")], "key 'A' already in Dict.frame")
})

test_that("[.Dict.frame can be converted to data.frame", {
    df = data.frame(A = 1:3, B = 4:6, C = 7:9)
    dif = dict.frame(df)
    expect_equal(as.data.frame(dif), df)
})

test_that("[.Dict.frame will signal if conversion to data.frame is not possible", {
    dif = dict.frame(list(i = identity, m = base::mean))
    expect_error(as.data.frame(dif),
                 "must consist of atomic columns")
})


test_that("[[<-.Dict.frame is consistent when trying to enter columns bad indices", {
    df = data.frame(A = 1:3, B = 4:6, C = 7:9)
    expect_error(df[, 5] <- 1, "would leave holes")
    expect_error(df[5] <- 1, "would leave holes")
    expect_error(df[[, 5]] <- 1)
    expect_silent(df[[5]] <- 1)  # strangely this works
    expect_warning(print(df))
    expect_equal(ncol(df), 5)
})

test_that("[[<-.Dict.frame operator works as expected", {
    df = data.frame(A = 1:3, B = 4:6, C = 7:9)
    dif = dict.frame(df)


    #dif[[2, 2]] <- 1


})

