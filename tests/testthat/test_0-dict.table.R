
describe("dict.table",
{
    data.table <- data.table::data.table

    it("can be created similarly to a data.table",
    {
        dat <- data.table(A = 1, B = 2)
        dit <- dict.table(A = 1, B = 2)
        expect_equivalent(dit, dat)
    })

    it("by default is initialized without checking of column names",
    {
        dit <- dict.table("x 1" = 1)
        expect_equal(colnames(dit), "x 1")
    })

    it("can be initialized with checking of column names",
    {
        dit <- dict.table("x 1" = 1, check.names = TRUE)
        expect_equal(colnames(dit), "x.1")
    })

    it("cannot be initialized with duplicated column names",
    {
        expect_error(
            dict.table(a = 1, b = 2, a = 3),
            "duplicated keys after init: a"
        )
    })

    it("table properties are consistent with data.table properties",
    {
        dat <- data.table(A = 1, B = 2)
        dit <- dict.table(A = 1, B = 2)
        expect_equal(dim(dit), dim(dat))
        expect_equal(nrow(dit), nrow(dat))
        expect_equal(ncol(dit), ncol(dat))
        expect_equal(rownames(dit), rownames(dat))
        expect_equal(colnames(dit), colnames(dat))
        expect_equal(dimnames(dit), dimnames(dat))
    })

    test_that("dict.table rownames can be changed",
    {
        dit <- dict.table(A = 1:3)
        expect_equal(row.names(dit), as.character(1:3))
        rownames(dit) <- letters[1:3]
        expect_equal(row.names(dit), letters[1:3])
    })

    test_that("dict.table is created as copy of another data.table",
    {
        dat <- data.table(A = 1)
        dat2 <- data.table::copy(dat)
        dit <- dict.table(dat)

        dat[1, 1] <- 9
        expect_false(dat == dat2)
        expect_true(dit == dat2)
    })

    test_that(
        "a column can be added if name is specified as character",
    {
        dit <- dict.table(A = 1)
        expect_false("x" %in% colnames(dit))
        expect_true(utils::hasName(add(dit, "b" = 2), "b"))
    })


    test_that("the length of added columns must match or be of length 1",
    {
        dit <- dict.table(A = 1:4, B = 4:1)
        expect_error(add(dit, a = 1:3),
                    "Supplied 3 items to be assigned to 4 items of column 'a'")
        expect_error(add(dit, a = 1:2))
        expect_false(has_name(dit, "a"))
        dit <- add(dit, a = 1)
        expect_equal(at2(dit, "a"), rep(1, 4))
    })


    test_that("a column cannot be added twice",
    {
        dit <- dict.table(A = 1:2, B = 2:1)
        expect_error(add(dit, "A" = 1:2), "name 'A' exists already")
    })

    test_that("it can be checked if a dict.table has a certain column",
    {
        dit <- dict.table(A = 1)
        expect_true(has_name(dit, "A"))
        expect_false(has_name(dit, "x"))
    })

    test_that("a dict.table can be cleared",
    {
        dit <- dict.table(A = 1, B = 2)
        expect_equal(clear(dit), dict.table())
    })

    test_that("columns can be deleted from dict.table",
    {
        dit <- dict.table(A = 1:2, B = 2:1, C = 3:4)
        expect_true(has_name(dit, "B"))
        ref_delete_at(dit, "B")
        expect_false(has_name(dit, "B"))

        expect_equal(c("A", "C"), colnames(dit))
        ref_delete_at(dit, "A", "C")
        expect_true(is_empty(dit))
    })

    test_that("columns can be discarded from dict.table",
    {
        dit <- dict.table(A = 1:2, B = 2:1, C = 3:4)
        expect_true(has_name(dit, "B"))
        ref_discard_at(dit, "B")
        expect_false(has_name(dit, "B"))

        expect_equal(c("A", "C"), colnames(dit))
        ref_discard_at(dit, c("A", "C"))
        expect_true(is_empty(dit))
    })

    test_that("columns can be discarded by index from dict.table",
    {
        dit <- dict.table(A = 1:2, B = 2:1, C = 3:4)
        expect_true(has_name(dit, "B"))
        ref_discard_at(dit, 2)
        expect_false(has_name(dit, "B"))
        expect_equal(c("A", "C"), colnames(dit))

        expect_true(is_empty(discard_at(dit, 1:2)))
    })

    test_that("discarding non-existing columns works as expected",
    {
        dit <- dict.table(A = 1:2, B = 2:1, C = 3:4)
        expect_true(is_empty(discard_at(dit, 1:10)))
        expect_equal(discard_at(dit, c("X", "Y")), dit)
    })

    test_that("deleting non-existing columns gives an error",
    {
        dit <- dict.table(A = 1:2, B = 2:1)
        dit.copy <- data.table::copy(dit)

        expect_error(delete_at(dit, "X"), "column\\(s\\) not found: 'X'")
        expect_error(delete_at(dit, 3), "index out of range \\(ncol = 2\\): 3")
        expect_true(identical(dit, dit.copy)) # dit.copy has not changed
    })

    test_that("dict.table has a size",
    {
        dit <- dict.table(A = 1:2, B = 2:1, C = 3:4)
        expect_equal(length(dit), ncol(dit))
        expect_equal(length(clear(dit)), 0)
    })

    test_that("standard dict.table getter works as expected",
    {
        dit <- dict.table(A = 1:2, B = 2:1, C = 3:4)
        daf = as.data.frame(dit)

        expect_equal(at2(dit, "A"), daf[["A"]])
        expect_equal(dit[["A"]], daf[["A"]])

        expect_error(at2(dit, c("A", "B")), "index must be of length 1")
    })

    test_that("peek works as expected",
    {
        dit <- dict.table(A = 1:2, B = 2:1)
        expect_equal(peek_at2(dit, "A"), at2(dit, "A"))
        expect_error(peek_at2(dit, c("A", "B")), "index must be of length 1")

        expect_true(is.null(peek_at2(dit, "X")))
        expect_equal(peek_at2(dit, "X", default = 9), rep(9, nrow(dit)))
        expect_equal(peek_at2(dit, "X", default = "a"), rep("a", nrow(dit)))
    })

    test_that("pop works as expected",
    {
        dit <- dict.table(A = 1:2, B = 2:1)
        expect_equal(ref_pop(dit, "A"), 1:2)
        expect_false(has_name(dit, "A"))

        expect_equal(ref_pop(dit, "B"), 2:1)
        expect_true(is_empty(dit))
    })

    test_that("columns can be renamed",
    {
        dit <- dict.table(A = 1:2, B = 2:1)
        ref_rename(dit, "A", "X")
        expect_equal(colnames(dit), c("X", "B"))
        ref_rename(dit, c("X", "B"), c("y", "z"))
        expect_equal(colnames(dit), c("y", "z"))
        expect_error(
            rename(dit, "A", "b"),
            "Items of 'old' not found in names: 'A'"
        )
    })

    test_that("a column can be set", {
        dit <- dict.table(A = 1:2, B = 2:1)
        dit <- replace_at(dit, "A", 3:4)
        expect_equal(dit[["A"]], 3:4)
    })

    test_that("a column can only bet set if already
        existing unless declared to be added",
    {
        dit <- dict.table(A = 1:2)
        expect_error(replace_at(dit, "B", 3:4), "column\\(s\\) not found: 'B'")

        ref_replace_at(dit, "B", 5:6, .add = TRUE)
        ref_replace_at(dit, "C" = 7:8, .add = TRUE)
        expect_equal(dit[["B"]], 5:6)
        expect_equal(colnames(dit), c("A", "B", "C"))

        expect_error(replace_at(dit, 4, 1:2, .add = TRUE),
                    "index out of range \\(ncol = 3\\): 4")
    })

    test_that("the length of set column must match or be of length 1",
    {
        dit <- dict.table(A = 1:4, B = 4:1)
        expect_error(
            replace_at(dit, "A", 1:3),
            "Supplied 3 items to be assigned to 4 items of column 'A'"
        )
        expect_error(replace_at(dit, "A", 1:2))
        expect_equal(dit[["A"]], 1:4)

        expect_equal(replace_at(dit, "A", 9)[["A"]], rep(9, 4))
    })

    test_that("a column can be set by numeric index",
    {
        dit <- dict.table(A = 1:2, B = 2:1)
        expect_equal(replace_at(dit, 2, 0)[[2]], rep(0, 2))
    })
})


describe("as.dict.table",
{
    data.table <- data.table::data.table

    test_that("dict.table coercion works as expected",
    {
        dit = dict.table(A = 1:2, B = 3:4)
        expect_true(is.data.frame(as.data.frame(dit)))

        dat = data.table::as.data.table(dit)
        expect_true(data.table::is.data.table(dat))
        expect_equal(dat, data.table(A = 1:2, B = 3:4))
        data.table::set(dat, j = "C", value = 1:2)
        expect_false(has_name(dit, "C"))

        daf <- as.data.frame(dit)
        expect_equal(daf, data.frame(A = 1:2, B = 3:4))
        expect_true(is.dict.table(dit))
    })
})




describe("is.dict.table",
{
    test_that("a dict.table is recognized as such",
    {
        dit <- dict.table(A = 1:2, B = 2:1)
        expect_true(is.dict.table(dit))
        expect_false(is.dict.table(data.frame(A = 1:2, B = 2:1)))
    })
})



describe("print.dict.table",
{
    test_that("printed dict.table has the expected header",
    {
        dit <- dict.table(A = 1:2, B = 2:1)
        out <- capture.output(print(dit))
        header <- out[1]
        expected_header <- "<dict.table> with 2 rows and 2 columns"
        expect_equal(header, expected_header)
    })
})


describe("rbind.dict.table",
{
    data.table <- data.table::data.table

    test_that("rbind works as expected",
    {
        dat <- data.table(A = 1:2, B = 2:1)
        dit <- dict.table(dat)
        dat2 <- rbind(dat, dat)
        dit2 <- rbind(dit, dit)
        expect_true(is.dict.table(dit2))
        expect_equal(data.table::as.data.table(dit2), dat2)
        dit.dat <- rbind(dit, dat)
        expect_true(is.dict.table(dit.dat))
        expect_equal(data.table::as.data.table(dit.dat), dat2)
    })
})


describe("cbind.dict.table",
{
    data.table <- data.table::data.table

    test_that("cbind works as expected for dict.tables",
    {
        dit <- dict.table(A = 1:2, B = 2:1)
        dit2 <- dict.table(C = 3:4, D = 5:6)
        expect_error(cbind(dit, dit), "found duplicated column names: A, B")
        expect_equal(
            cbind(dit, dit2),
            dict.table(A = 1:2, B = 2:1, C = 3:4, D = 5:6)
        )
        expect_equal(cbind(dit[, 1], dit[, 2]), dit)
    })


    test_that("cbind works with data.tables",
    {
        expect_equal(
            cbind(dict.table(a = 1:2), data.table(b = 1:2)),
            dict.table(a = 1:2, b = 1:2)
        )

        expect_equal(
            cbind(data.table(a = 1:2), dict.table(b = 1:2)),
            data.table(a = 1:2, b = 1:2)
        )
    })
})
