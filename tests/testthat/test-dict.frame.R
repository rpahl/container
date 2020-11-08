context("Dict.frame")

test_that("Dict.frame constructor works as expected", {
    # initialize
    expect_error(Dict.frame$new(1:2), "all items must be named")
    expect_equal(Dict.frame$new()$keys(), character(0))
    d <- Dict.frame$new(c(x=1L, y=2L))
    expect_equal(d$nrow(), 1)
    expect_equal(d$ncol(), 2)

    expect_true(inherits(d, "Dict.frame"))
    expect_true(inherits(d, "Dict"))
    expect_true(inherits(d, "Container"))

    expect_error(d$get("z"), "key 'z' not in Dict.frame")
    expect_error(d$add(key="", 3L, "zero-length key"))
    expect_equal(as.integer(d$values()), 1:2)
    expect_equal(d$type(), "integer")
    expect_equal(Dict.frame$new()$type(), "list")
    expect_error(Dict.frame$new(list(x=1, y=2, x=3)), "duplicated keys")
})


test_that("adding and setting columns to Dict.frame works as expected", {
    df <- data.frame(A = 1:2, B = letters[1:2])
    dif <- Dict.frame$new(df[, 1, drop = FALSE])

    expect_equal(as.data.frame(dif$values()), df[, 1, drop = FALSE])
    dif$add("B", df[, 2])
    expect_equal(as.data.frame(dif$values()), df)

    expect_error(dif$add("B", df[, 2]), "key 'B' already in Dict.frame")

    expect_error(dif$set("C", 3:4), "key 'C' not in Dict.frame")
    dif$set("C", 3:4, add = TRUE)
    expect_equal(dif$get("C"), 3:4)

    dif$set("C", 5:6)
    expect_equal(dif$get("C"), 5:6)

    expect_error(dif$set("C", 5:7), "elements must be of length 2")
})

test_that("removing columns from Dict.frame works as expected", {
    df <- data.frame(A = 1:2, B = letters[1:2])
    dif <- Dict.frame$new(df)
    expect_equal(dif$ncol(), 2)
    expect_error(dif$remove("C"), "key 'C' not in Dict.frame")
    dif$discard("C")
    expect_equal(dif$ncol(), 2)

    dif$discard("B")
    expect_equal(dif$ncol(), 1)
    dif$remove("A")
    expect_equal(dif$ncol(), 0)

})

test_that("added columns must match in length except if dict.frame still empty", {
    dif <- Dict.frame$new()
    dif$add("A", 1:2)
    expect_equal(dif$nrow(), 2)
    expect_error(dif$add("B", 1:3), "elements must be of length 2")
})

test_that("dict.frame remembers number of rows even after clearance", {
    # Note: this is consistent with base data.frame
    df = data.frame(A = 1:3)
    dif <- Dict.frame$new(df)
    expect_equal(dif$nrow(), 3)
    dif$clear()
    expect_equal(dif$ncol(), 0)
    expect_equal(dif$nrow(), 3)
    df[["A"]] <- NULL
    expect_equal(ncol(df), 0)
    expect_equal(nrow(df), 3)
})


test_that("Dict.frame update works as expected", {
    df1 <- data.frame(A = 1:2, B = letters[1:2])
    df2 <- data.frame(A = 2:1, C = letters[1:2])
    dif1 <- Dict.frame$new(df1)
    dif2 <- Dict.frame$new(df2)

    df12 = as.data.frame(dif1$update(dif2)$values())
    df.expected = data.frame(A = 2:1, B = letters[1:2], C = letters[1:2])
    expect_equal(df12, df.expected)

    expect_error(dif1$update(data.frame(A = 1:3)), "arg must be a Dict.frame")
    expect_error(dif1$update(Dict.frame$new(data.frame(A = 1:3))),
                 "elements must be of length 2")
})


test_that("Dict.frame is printed as data.frame if possible", {
    df = data.frame(A = 1:2, B = 1:2)
    dif <- Dict.frame$new(df)

    tmp1 = tempfile()
    on.exit(file.remove(tmp1), add = TRUE)
    sink(tmp1)
    print(df)
    sink()

    tmp2 = tempfile()
    on.exit(file.remove(tmp2), add = TRUE)
    sink(tmp2)
    dif$print()
    sink()

    out1 = readLines(tmp1)
    out2 = readLines(tmp2)

    header = out2[1]
    expect_equal(out1, out2[-1])
    expect_equal(header, "<Dict.frame> with 2 columns and 2 rows")
})
