context("dict.frame S3")

test_that("dict.frame creation works as expected", {
    dif.df = as.data.frame(dict.frame())
    attr(dif.df, "row.names") <- integer(0)
    expect_equal(dif.df, data.frame())

    dif.df = as.data.frame(dict.frame(data.frame()))
    attr(dif.df, "row.names") <- integer(0)
    expect_equal(dif.df, data.frame())

    dif.df = as.data.frame(dict.frame(list()))
    attr(dif.df, "row.names") <- integer(0)
    expect_equal(dif.df, data.frame())

    df = data.frame(A = 1:2, B = 3:4)
    expect_equal(as.data.frame(dict.frame(A = 1:2)), df["A"])
    expect_equal(as.data.frame(dict.frame(A = 1:2, B = 3:4)), df)
    expect_equal(as.data.frame(dict.frame(df)), df)

    expect_equal(dict.frame(list(A = 1:3, B = 4:6)),
                 dict.frame(A = 1:3, B = 4:6))

    expect_error(dict.frame(A = 1:3, 3:4), "all items must be named")
    expect_error(dict.frame(A = 1:3, 1:10), "all items must be named")
    expect_error(dict.frame(A = 1:3, B = 1:10),
                 "All elements must have the same length.")
})


test_that("[[.Dict.frame operator extracts values as expected", {
    df = data.frame(A = 1:3, B = 4:6)

    dif = dict.frame(A = 1:3, B = 4:6)
    expect_error(dif[[]], '"i" is missing')
    expect_error(dif[[1, ]], '"j" is missing')
    expect_error(dif[[, 1]], '"i" is missing')

    expect_error(dict.frame(A = 1:2, B = 1:3),
                 "All elements must have the same length.")

    expect_equal(dif[[1, 2]], df[[1, 2]])
    expect_equal(dif[[2, "A"]], df[[2, "A"]])
    expect_equal(dif[[1]], df[[1]])
    expect_equal(dif[["A"]], df[["A"]])
})

test_that("[[.Dict.frame can be initialized from data.frame", {
    df = data.frame(A = 1:3, B = 4:6)
    dif = dict.frame(df)
    expect_equal(as.data.frame(dif), df)
})


test_that("[[.Dict.frame can have arbitraty colum names", {
    dif = dict.frame("Hi !" = letters[1:3], "123" = 1:3)
    expect_equal(dif[["123"]], 1:3)
    expect_equal(dif[["Hi !"]], c("a", "b", "c"))
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

    expect_error(dif[[1:2, 1]], "i must be of length 1")
    expect_error(dif[[1, 1:2]], "j must be of length 1")

    # Column undefined index
    expect_equal(dif[["X"]], df[["X"]])
    expect_true(is.null(dif[["X"]]))
    expect_equal(dif[["X", default = 1]], c(1, 1))

    expect_true(is.null(dif[[1, "X"]]))
    expect_true(is.null(df[[1, "X"]]))
    expect_equal(dif[[1, "X", default = 9]], 9)

    # Column out of range
    expect_error(dif[[3]], "column index out of bounds: 3")
    expect_error(dif[[3, default = 1]], "column index out of bounds: 3")
    expect_error(df[[3]], "subscript out of bounds")

    expect_error(dif[[1, 3]], "column index out of bounds: 3")
    expect_error(dif[[1, 3, default = 1]], "column index out of bounds: 3")
    expect_error(df[[1, 3]], "subscript out of bounds")

    # Row out of range
    expect_equal(dif[[5, "X"]], df[[5, "X"]])
    expect_true(is.null(dif[[5, "X"]]))
    expect_error(dif[[5, "X", default = 1]], "row index out of bounds: 5")

    expect_error(dif[[5, 1]], "row index out of bounds: 5")
    expect_error(df[[5, 1]], "subscript out of bounds")
    expect_error(dif[[5, 1, default = 1]], "row index out of bounds: 5")

    # Row and column out of range
    expect_error(dif[[3, 4]], "column index out of bounds: 4")
    expect_error(dif[[3, 4, default = 9]], "column index out of bounds: 4")
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
    df = data.frame(A = 1:3, B = 4:6)
    dif = dict.frame(df)

    expect_true(is.dict.frame(dif[, 1]))
    expect_true(is.dict.frame(dif[, "B"]))
    expect_true(is.dict.frame(dif[1, ]))
    expect_true(is.dict.frame(dif[, ]))
    expect_equal(dif[], dif)
    expect_equal(dif[, ], dif)
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
    expect_error(dif[3], "column index out of bounds: 3")
    expect_error(df[3], "undefined columns selected")
    expect_error(dif[3, default = 1], "column index out of bounds: 3")

    expect_error(dif[1, 3], "column index out of bounds: 3")
    expect_error(dif[1, 3, default = 9], "column index out of bounds: 3")
    expect_error(dif[1, 3, default = 1:2], "column index out of bounds: 3")
    expect_error(dif[1:2, 3], "column index out of bounds: 3")
    expect_equal(df[1, 3], NULL) # inconsistent data.frame behaviour

    expect_error(dif[, 3], "column index out of bounds: 3")
    expect_error(dif[, 3, default = 9], "column index out of bounds: 3")
    expect_error(df[, 3], "undefined columns selected")

    expect_error(dif[, 1:3], "column index out of bounds: 3")
    expect_error(dif[, 1:3, default = 9], "column index out of bounds: 3")
    expect_error(df[, 1:3], "undefined columns selected")

    # Row out of range
    expect_error(dif[5, 1], "row indices out of bounds: 5")
    expect_error(dif[1:5, 1], "row indices out of bounds: 3, 4, 5")
    expect_error(dif[5, "X", default = 1], "row indices out of bounds: 5")
    expect_error(dif[1:5, "X", default = 1], "row indices out of bounds: 3, 4, 5")

    # Row and column out of range
    expect_error(dif[5, "X"], "column 'X' not found")
    expect_error(dif[5, 5], "column index out of bounds: 5")
    expect_error(dif[5, 5, default = 1], "column index out of bounds: 5")
})

test_that("[.Dict.frame operator can provide default values", {
    df = data.frame(A = 1:3, B = 4:6, C = 7:9)
    dif = dict.frame(df)

    expect_equal(as.data.frame(dif[1:2, c("B", "X"), default = 0]),
                 data.frame(B = 4:5, X = 0))
    expect_equal(as.data.frame(dif[1:2, c("B", "X"), default = 7:9]),
                 data.frame(B = 4:5, X = 7:8))

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


test_that("[[<-.Dict.frame operator behaves as expected for out of range indices", {
    df = data.frame(A = 1:2, B = 3:4)
    dif = dict.frame(A = 1:2, B = 3:4)

    expect_error(dif[[1, 1]] <- 1:2, "value must be of length 1")
    expect_error(dif[[1, 1]] <- NULL, "value must be of length 1")
    expect_error(dif[[1:2, 1]] <- 1, "i must be of length 1")
    expect_error(dif[[1, 1:2]] <- 1, "j must be of length 1")

    # Column undefined index
    expect_error(dif[["X"]] <- 1, "column 'X' not found")
    expect_silent(dif[["X", add = TRUE]] <- 1)
    expect_silent(df[["X"]] <- 1)
    expect_equal(df, as.data.frame(dif))

    expect_error(dif[[1, "Z"]] <- 9, "column 'Z' not found")
    expect_error(dif[[1, "Z", add = TRUE]] <- 9, "column 'Z' not found")
    expect_error(df[[1, "Z"]] <- 9, "replacing element in non-existent column: Z")

    # Column out of range
    expect_error(dif[[4]] <- 9, "column index out of bounds: 4")
    expect_error(dif[[4, add = TRUE]] <- 9, "column index out of bounds: 4")
    expect_silent(df[[4]] <- 9)  # works
    expect_silent(df[[4]] <- NULL)

    expect_error(dif[[1, 4]] <- 9, "column index out of bounds: 4")
    expect_error(dif[[1, 4, add = TRUE]] <- 9, "column index out of bounds: 4")
    expect_error(df[[1, 4]] <- 9, "replacing element in non-existent column: 4")

    # Row out of range
    expect_error(dif[[5, "X"]] <- 1, "row indices out of bounds: 5")
    expect_error(dif[[5, "X", add = TRUE]] <- 1, "row indices out of bounds: 5")
    expect_silent(df[[5, "X"]] <- 1)

    expect_error(dif[[5, 1]] <- 1, "row indices out of bounds: 5")
    expect_error(dif[[5, 1, add = TRUE]] <- 1, "row indices out of bounds: 5")
    df = data.frame(A = 1:2, B = 3:4)
    expect_silent(df[[5, 1]] <- 1)

    # Row and column out of range
    expect_error(dif[[3, 4]] <- 1, "column index out of bounds: 4")
    expect_error(dif[[3, 4, add = TRUE]] <- 1, "column index out of bounds: 4")
})


test_that("[[<-.Dict.frame operator behaves as expected", {
    dif = dict.frame(A = 1:2, B = 3:4)

    dif[[1, 1]] <- 9
    expect_equal(dif[[1, 1]], 9)

    dif[[1, "A"]] <- 0
    expect_equal(dif[[1, "A"]], 0)

    dif[[2]] <- 9
    expect_equal(dif[[2]], c(9, 9))

    dif[["B"]] <- 0
    expect_equal(dif[["B"]], c(0, 0))

    dif = dict.frame(A = 1:3, B = 4:6)
    expect_error(dif[["A"]] <- 1:2, "must be a multiple")
    dif[["A"]] <- 10:12
    expect_equal(dif[["A"]], 10:12)

    before = clone(dif)
    expect_error(dif[["D"]] <- NULL, "column 'D' not found")
    expect_silent(dif[["D", add = TRUE]] <- NULL)
    after = clone(dif)
    expect_equal(before, after)

    dif[["B"]] <- NULL
    expect_equal(dif, before["A"])

    dif[[1]] <- NULL
    expect_equal(dim(dif), c(3, 0))

    expect_error(dif[["C", add = TRUE]] <- 1:2)
    expect_silent(dif[["C", add = TRUE]] <- 1:3)
})

test_that("[[<-.Dict.frame operator works with empty dict.frame", {
    df = data.frame()
    expect_error(df[["A"]] <- 1)
    expect_error(df[[1]] <- 1)
    expect_error(df[[1, 1]] <- 1)

    dif = dict.frame()
    expect_error(dif[[1, 1, add = TRUE]] <- 1, "column index out of bounds: 1")
    expect_error(dif[[1, "A", add = TRUE]] <- 1, "column 'A' not found")
    expect_error(dif[["A"]] <- 1:10, "column 'A' not found")
    dif[["A", add = TRUE]] <- 1:10
    expect_equal(dim(dif), c(10, 1))
})


test_that("[<-.Dict.frame operator behaves as expected for out of range indices", {
    df = data.frame(A = 1:3, B = 4:6)
    dif = dict.frame(A = 1:3, B = 4:6)

    # Column undefined index
    expect_error(dif["X"] <- 1, "column 'X' not found")
    expect_silent(dif["X", add = TRUE] <- 1)
    expect_silent(df["X"] <- 1)
    expect_equal(df, as.data.frame(dif))

    expect_error(dif[1, "Z"] <- 9, "column 'Z' not found")
    expect_error(dif[1, "Z", add = TRUE] <- 9, "column 'Z' not found")
    expect_error(dif[1:3, "Z"] <- 9, "column 'Z' not found")
    expect_silent(dif[1:3, "Z", add = TRUE] <- 9)

    expect_silent(df[1, "Z"] <- 9) # works for basic data.frames
    expect_equal(df[, "Z"], c(9, NA, NA))
    expect_silent(df["Z"] <- 9)
    expect_equal(as.data.frame(dif), df)

    # Column out of range
    expect_error(dif[7] <- 9, "column index out of bounds: 7")
    expect_error(dif[7, add = TRUE] <- 9, "column index out of bounds: 7")
    expect_error(df[7] <- 9, "new columns would leave holes")
    expect_silent(df[[7]] <- 9) # works - auto-fills holes
    expect_equal(ncol(df), 7)
    expect_warning(expect_output(print(df)), "corrupt data frame")

    expect_error(dif[1, 7] <- 9, "column index out of bounds: 7")
    expect_error(dif[1, 7, add = TRUE] <- 9, "column index out of bounds: 7")

    dif = dict.frame(A = 1:3, B = 4:6)
    df = data.frame(A = 1:3, B = 4:6)
    expect_error(df[1, 7] <- 9, "new columns would leave holes")

    # Row out of range
    expect_error(dif[5, "B"] <- 1, "row indices out of bounds: 5")
    expect_error(dif[5, "B", add = TRUE] <- 1, "row indices out of bounds: 5")
    expect_silent(df[5, "B"] <- 1) # auto-fills rows with NA
    expect_equal(nrow(df), 5)

    expect_error(dif[5, 1] <- 1, "row indices out of bounds: 5")
    expect_error(dif[5, 1, add = TRUE] <- 1, "row indices out of bounds: 5")
    df = data.frame(A = 1:2, B = 3:4)
    expect_silent(df[5, 1] <- 1) # auto-fills rows with NA
    expect_equal(nrow(df), 5)

    expect_error(dif[5, ] <- 1, "row indices out of bounds: 5")
    df = data.frame(A = 1:2, B = 3:4)
    expect_silent(df[5, ] <- 1)
    expect_equal(nrow(df), 5)

    # Row and column out of range
    expect_error(dif[3, 7] <- 1, "column index out of bounds: 7")
    expect_error(dif[3, 7, add = TRUE] <- 1, "column index out of bounds: 7")
})


test_that("[<-.Dict.frame operator works as expected", {
    dif = dict.frame(A = 1:3, B = 4:6, C = 7:9)

    expect_error(dif[1, 1] <- NULL, "length of value must .* match number of rows")
    expect_error(dif[1, 1] <- 1:2, "length of value must .* match number of rows")
    expect_error(dif[, 1] <- 1:2, "number of values must be a multiple")
    expect_silent(dif[, 1] <- 9)
    expect_equal(dif[[1]], rep(9, 3))
    expect_silent(dif[c(1, 3), 1] <- 7)
    expect_equal(dif[[1]], c(7, 9, 7))

    expect_silent(dif[c(1, 3), c(1, 3)] <- 0)
    expect_equal(dif[[1]], c(0, 9, 0))
    expect_equal(dif[[2]], c(4, 5, 6))
    expect_equal(dif[[3]], c(0, 8, 0))

    expect_silent(dif[1:3, 2] <- 0)
    expect_equal(dif[[2]], rep(0, 3))

    expect_silent(dif[1:2, "A"] <- -1)
    expect_equal(dif[["A"]], c(-1, -1, 0))

    expect_silent(dif[c(1, 3), c("A", "C")] <- 10)
    expect_equal(dif[["A"]], c(10, -1, 10))
    expect_equal(dif[["B"]], c(0, 0, 0))
    expect_equal(dif[["C"]], c(10, 8, 10))

    dif[1:2, 1:2] <- 1:2
    expect_equal(dif[[1]][1:2], 1:2)
    expect_equal(dif[[2]][1:2], 1:2)

    dif[2, ] <- 11
    expect_equal(as.numeric(values(dif[2, ])), rep(11, 3))
})

test_that("[<-.Dict.frame operator works with mixed subscripts", {
    dif = dict.frame(A = 1:3, B = 4:6, C = 7:9)
    expect_silent(dif[c(1, 3), list("C", 2)] <- 0)
    expect_equal(as.numeric(values(dif[1, 2:3])), c(0, 0))
    expect_equal(as.numeric(values(dif[3, 2:3])), c(0, 0))
})

test_that("Dict.frame row can be converted to numeric", {
    dif = dict.frame(A = 1:3, B = 4:6, C = 7:9)
    expect_error(as.numeric(dif), "must consist of one row or column")

    v = as.numeric(dif[1, ])
    expect_equal(names(v), c("A", "B", "C"))
    expect_equal(as.numeric(v), c(1, 4, 7))

    expect_equal(as.numeric(dif[, 2]), 4:6)

})

test_that("Dict.frame print works as expected", {
    dif = dict.frame(A = 1:20, B = 20:1)

    res = expect_output(print(dif))
    expect_equal(res[["A"]], c(1:5, ".", 16:20))
    expect_equal(res[["B"]], c(20:16, ".", 5:1))
    expect_equal(rownames(res), c(1:5, ".", 16:20))

    res = expect_output(print(dif, len = 3))
    expect_equal(res[["A"]], c(1, ".", 20))
    expect_equal(res[["B"]], c(20, ".", 1))
    res2 = expect_output(print(dif, len = 2))
    expect_equal(res, res2)

    expect_error(print(dif, len = 1), "len must be > 1")

    dif2 = dif[, "A"]
    dif2[["A"]] = NULL
    res = expect_output(print(dif2))
    expect_equal(dim(res), c(0, 0))
})


test_that("Dict.frame can handle complex objects ", {
    df = data.frame(A = 1:2)
    dif = dict.frame(df)

    dif["df", add = TRUE] <- list(df)
    dif["f", add = TRUE] <- list(base::mean)
    dif[2, "f"] <- stats::median
    expect_equal(dif$apply(typeof),
                 list("A" = "integer", "df" = "list", "f" = "list"))

    expect_equal(dif[[1, "f"]](c(1, 2, 10)), mean(c(1, 2, 10)))
    expect_equal(dif[[2, "f"]](c(1, 2, 10)), median(c(1, 2, 10)))
})


test_that("dict.frames can be row-binded", {
    df = data.frame(A = 1:2, B = 3:4)
    dif = dict.frame(df)

    expect_equal(as.data.frame(rbind(dif, dif, dif)), rbind(df, df, df))

    dif["df", add = TRUE] <- list(df)
    dif["f", add = TRUE] <- list(base::mean)

    expect_equal(dim(rbind(dif, dif)), c(4, 4))
})

test_that("nrow, ncol, and dim can be called on dict.frames", {
    df = data.frame(A = 1:10)
    dif = dict.frame(df)
    expect_equal(nrow(df), nrow(dif))
    expect_equal(ncol(df), ncol(dif))
    expect_equal(dim(df), dim(dif))
})

test_that("row and column names can be retrieved and set for dict.frames", {
    df = data.frame(A = 1:2, B = 3:4)
    dif = dict.frame(df)
    expect_equal(as.character(rownames(dif)), rownames(df))
    expect_equal(colnames(dif), colnames(df))

    expect_equal(dimnames(dif), dimnames(df))

    colnames(dif) <- c("x", "y")
    expect_equal(dif$keys(), c("x", "y"))

    rownames(dif) <- 3:4
    expect_equal(rownames(dif), c("3", "4"))

    row.names(dif) <- letters[1:2]
    expect_equal(row.names(dif), letters[1:2])
})


test_that("setval works as expected", {
    df = data.frame(A = 1:2, B = 3:4)
    dif = dict.frame(df)
    expect_error(setval(dif, "C", 1:4), "elements must be of length 2")
    expect_error(setval(dif, "C", 1:2), "key 'C' not in Dict.frame")
    setval(dif, "C", 1:2, add = TRUE)
    expect_equal(dif$get("C"), 1:2)

    setval(dif, "B", 1:2)
    expect_equal(dif$get("B"), 1:2)
})

