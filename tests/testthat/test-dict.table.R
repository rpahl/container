context("Dict.table")

test_that("Dict.table constructor works as expected", {
    # initialize
    expect_error(Dict.table$new(1:2), "all elems must be named")
    expect_equal(Dict.table$new()$keys(), character(0))
    expect_equal(Dict.table$new()$values(), data.table())

    dat <- data.table(x = 1:2, y = 3:4)
    dit <- Dict.table$new(x = 1:2, y = 3:4)
    expect_equal(dit$nrow(), nrow(dat))
    expect_equal(dit$ncol(), ncol(dat))
    expect_equal(dit$dim(), dim(dat))

    expect_true(inherits(dit, "Dict.table"))
    expect_true(inherits(dit, "Dict"))
    expect_true(inherits(dit, "Container"))

    expect_equal(dit$type(), "list")
    expect_equal(Dict.table$new()$type(), "list")
    expect_error(Dict.table$new(x = 1, y = 2, x = 3), "duplicated keys")

    expect_equal(Dict.table$new(a = 1:2, b = 3:4),
                 Dict.table$new(list(a = 1:2, b = 3:4)))
})

test_that("Dict.table can be created from data.table object", {
    dat <- data.table(A = 1:3)
    dit <- Dict.table$new(dat)
    expect_true(identical(dit$values(), dat))
})

test_that("Default constructor has copy semantics", {
    dat <- data.table(A = 1)
    dit <- Dict.table$new(dat)

    data.table::set(dat, j = "x", value = 2)
    expect_true("x" %in% colnames(dat))
    expect_false(dit$has("x"))
    dit$add("y", 2)
    expect_false("y" %in% colnames(dat))
    expect_true(dit$has("y"))

    dat <- data.table(A = 1)
    dit <- Dict.table$new(dat)
    data.table::set(dat, i = 1L, j = "A", value = 9)
    expect_true(dat[["A"]][1] == 9)
    expect_false(dit[["A"]][1] == 9)
    dit$setval("A", 7)
    expect_false(dat[["A"]][1] == 7)
    expect_true(dit[["A"]][1] == 7)
})


test_that("Dict.table objects with reference to passed data.table object can be created", {
    dat <- data.table(A = 1)
    dit <- Dict.table$new(dat, asis = TRUE)

    data.table::set(dat, j = "x", value = 2)
    expect_true("x" %in% colnames(dat))
    expect_true(dit$has("x"))
    dit$add("y", 2)
    expect_true("y" %in% colnames(dat))
    expect_true(dit$has("y"))

    dat <- data.table(A = 1)
    dit <- Dict.table$new(dat, asis = TRUE)
    data.table::set(dat, i = 1L, j = "A", value = 9)
    expect_true(dat[["A"]][1] == 9)
    expect_true(dit[["A"]][1] == 9)
    dit$setval("A", 7)
    expect_true(dat[["A"]][1] == 7)
    expect_true(dit[["A"]][1] == 7)

    dat[1, 1] <- 3 # creates a copy
    expect_true(dat[["A"]][1] == 3)
    expect_true(dit[["A"]][1] == 7)
})

test_that("adding and setting columns to Dict.table works as expected", {
    daf <- data.frame(A = 1:2, B = letters[1:2])
    dit <- Dict.table$new(daf[, 1, drop = FALSE])
    expect_error(dit$add(key="", 3L), "zero-length key")

    expect_equal(as.data.frame(dit$values()), daf[, 1, drop = FALSE])
    dit$add("B", daf[, 2])
    expect_equal(as.data.frame(dit$values()), daf)

    expect_error(dit$add("B", daf[, 2]), "key 'B' already in Dict.table")

    expect_error(dit$setval("C", 3:4), "key 'C' not in Dict.table")
    dit$setval("C", 3:4, add = TRUE)
    expect_equal(dit$getval("C"), 3:4)

    dit$setval("C", 5:6)
    expect_equal(dit$getval("C"), 5:6)

    expect_error(dit$setval("C", 7:9),
                 "Supplied 3 items to be assigned to 2 items of column 'C'")
})

test_that("deleting columns from Dict.table works as expected", {
    daf <- data.frame(A = 1:2, B = letters[1:2])
    dit <- Dict.table$new(daf)
    expect_equal(dit$ncol(), 2)
    expect_error(dit$delete("X"), "Column 'X' not in Dict.table")
    expect_error(dit$delete(c("A", "X", "Y")),
                 "Columns 'X', 'Y' not in Dict.table")
    expect_error(dit$delete(3), "Column '3' out of range")
    expect_error(dit$delete(1:5), "Columns '3', '4', '5' out of range")

    dit$delete("B")
    expect_equal(dit$ncol(), ncol(daf) - 1)
    dit$delete(1)
    expect_true(dit$empty())

    dit <- Dict.table$new(daf)
    dit$delete(1:2)
    expect_true(dit$empty())

    dit <- Dict.table$new(daf)
    dit$delete(2:1)
    expect_true(dit$empty())

    dit <- Dict.table$new(daf)
    dit$delete(c("A", "B"))
    expect_true(dit$empty())

    dit <- Dict.table$new(daf)
    dit$delete(c("A", "A", "B"))
    expect_true(dit$empty())

    dit <- Dict.table$new(daf)
    dit$delete(c(1, 1, 2))
    expect_true(dit$empty())
    expect_error(dit$delete(1))
})

test_that("discarding columns from Dict.table works as expected", {
    daf <- data.frame(A = 1:2, B = letters[1:2])
    dit <- Dict.table$new(daf)
    expect_equal(dit$ncol(), ncol(daf))
    expect_silent(dit$discard("D"))
    expect_silent(dit$discard(4))
    expect_equal(dit$ncol(), ncol(daf))

    dit$discard("B")
    expect_equal(dit$ncol(), ncol(daf) - 1)
    dit$discard(1)
    expect_true(dit$empty())

    dit <- Dict.table$new(daf)
    dit$discard(1:10)
    expect_true(dit$empty())

    dit <- Dict.table$new(daf)
    dit$discard(2:1)
    expect_true(dit$empty())

    dit <- Dict.table$new(daf)
    dit$discard(c(1, 1, 1, 2, 2, 3))
    expect_true(dit$empty())

    dit <- Dict.table$new(daf)
    dit$discard(c("A", "B", "A", "B"))
    expect_true(dit$empty())

    dit <- Dict.table$new(daf)
    dit$discard(c("A", "A", "A"))
    expect_true(dit$has("B"))
})

test_that("columns can be added to empty Dict.table", {
    dat <- data.table()
    expect_error(dat[["A"]] <- 1:2)
    data.table::set(dat, j = "A", value = 1:2)

    dit <- Dict.table$new()
    dit$add("A", 1:2)
    expect_equal(dit$values(), dat)
})


test_that("one or more Dict.table objects can be row-binded", {
    daf = data.frame(A = 1:2, B = 3:4)
    dit = Dict.table$new(daf)

    expect_equal(dit$rbind(dit), Dict.table$new(rbind(daf, daf)))

    expect_equal(dit$rbind(dit)$rbind(dit), Dict.table$new(rbind(daf, daf, daf)))
})

test_that("keys correspond to column names", {
    dat = data.table(A = 1:2, B = 3:4)
    dit = Dict.table$new(dat)
    expect_equal(dit$keys(), colnames(dat))
})

test_that("columns can be retrieved", {
    dat = data.table(A = 1:2, B = 3:4)
    dit = Dict.table$new(dat)
    expect_equal(dit$getval("A"), dat[["A"]])

    expect_error(dit$getval(c("A", "B")), "key must be of length 1")
    expect_error(dit$getval("X"), "key 'X' not in Dict.table")
})

test_that("columns can be peeked with default value", {
    dat = data.table(A = 1:2, B = 3:4)
    dit = Dict.table$new(dat)
    expect_equal(dit$peek("A"), dat[["A"]])
    expect_error(dit$peek(c("A", "B")), "key must be of length 1")

    expect_equal(dit$peek("C"), dat[["C"]])
    expect_equal(dit$peek("C", default = 99), c(99, 99))
})

test_that("columns can be popped", {
    dat = data.table(A = 1:2, B = 3:4)
    dit = Dict.table$new(dat)

    res = dit$pop("A")
    expect_equal(res, dat[["A"]])
    expect_equal(dit$values(), dat[, -"A"])
    expect_error(dit$pop("A"), "Column 'A' not in Dict.table")
    expect_error(dit$pop(2), "Column '2' out of range")

    expect_error(dit$pop(1:2), "key must be of length 1")
    res = dit$pop(1)
    expect_equal(res, dat[[2]])
    expect_true(dit$empty())
})

test_that("popitem works as expected", {
    dat = data.table(A = 1, B = 2)
    dit = Dict.table$new(A = 1, B = 2)

    expect_true(any(sapply(dat, identical, y = dit$popitem())))
    expect_equal(dit$ncol(), 1)
    expect_true(any(sapply(dat, identical, y = dit$popitem())))
    expect_true(dit$empty())
})

test_that("columns/keys can be renamed", {
    dit = Dict.table$new(A = 1, B = 2)
    expect_error(dit$rename(1, "C"), "old must be character")
    expect_error(dit$rename("A", 1), "new must be character")
    expect_error(dit$rename("A", c("C", "D")), "must be of same length")
    expect_error(dit$rename("A", "B"), "rename failed because 'B' exists already")
    expect_error(dit$rename("Z", "B"), "key 'Z' not found")

    values = as.numeric(dit$values())
    dit$rename("A", "a")
    expect_true(dit$has("a"))
    expect_false(dit$has("A"))

    # Verify that values did not change
    expect_equal(values, as.numeric(dit$values()))

    # Several keys at once
    dit$rename(c("a", "B"), c("x", "y"))
    expect_equal(dit$keys(), c("x", "y"))

    # Renaming same key multiple times is possible
    dit$rename(c("x", "x2"), c("x2", "x3"))
    expect_equal(dit$keys(), c("x3", "y"))
})

test_that("columns can be sorted", {
    dit = Dict.table$new(A = 1, B = 2)
    dit2 = Dict.table$new(B = 2, A = 1)
    expect_false(identical(dit$values(), dit2$values()))

    dit$sortkey(decr = TRUE)
    expect_true(identical(dit$values(), dit2$values()))
})

test_that("Dict.table update works as expected", {
    daf1 <- data.frame(A = 1:2, B = letters[1:2])
    daf2 <- data.frame(A = 2:1, C = letters[1:2])
    dit1 <- Dict.table$new(daf1)
    dit2 <- Dict.table$new(daf2)

    daf12 = as.data.frame(dit1$update(dit2)$values())
    daf.expected = data.frame(A = 2:1, B = letters[1:2], C = letters[1:2])
    expect_equal(daf12, daf.expected)

    expect_error(dit1$update(data.frame(A = 1:3)), "arg must be a Dict.table")
    expect_error(dit1$update(Dict.table$new(data.frame(A = 1:3))),
                 "Supplied 3 items to be assigned to 2 items of column 'A'")
})

