describe(
    "rename functions",
{
    ee <- expect_equal

    describe(
        "rename.Container",
    {
        test_that("rename validates input correctly",
        {
            x <- container(A = 1, B = 2)
            expect_error(rename(x, 1, "C"), "'old' must be character")
            expect_error(rename(x, "A", 1), "'new' must be character")
            expect_error(
                rename(x, "A", c("C", "D")),
                "must be of the same length"
            )
            expect_error(rename(x, "A", "B"), "name 'B' already in Container")
            expect_error(
                rename(x, "Z", "B"),
                "Items of 'old' not found in names: 'Z'"
            )
            expect_error(rename(x, c("A", "A"), c("a", "a")),
                         "'old' has duplicated names: 'A'"
            )
        })

        test_that("rename works correctly with value semantics",
        {
            x <- container(A = 1, B = 2)
            vals <- as.numeric(as.list(x))
            ee(rename(x, "A", "a"), container(a = 1, B = 2))
            ee(x, container(A = 1, B = 2))   # names were changed by value
        })

        test_that("ref_rename works correctly with reference semantics",
        {
            x <- container(A = 1, B = 2)
            vals <- as.numeric(as.list(x))
            ref_rename(x, "A", "a")
            ee(x, container(a = 1, B = 2)) # now names were changed by reference

            expect_true(has_name(x, "a"))
            expect_false(has_name(x, "A"))

            # Verify that values did not change
            expect_equal(vals, as.numeric(as.list(x)))
        })

        test_that("rename handles multiple keys correctly",
        {
            x <- container(a = 1, B = 2)
            # Several keys at once
            ee(rename(x, c("a", "B"), c("x", "y")), container(x = 1, y = 2))

            # Renaming same key multiple times is not possible
            x <- container(x = 1, y = 2)
            expect_error(rename(x, c("x", "x2"), c("x2", "x3")),
                         "Items of 'old' not found in names: 'x2'")
        })
    })

    describe(
        "rename.dict.table",
    {
        test_that("rename works correctly on dict.table",
        {
            dit <- dict.table(A = 1:2, B = 2:1)
            ee(rename(dit, "A", "X"), dict.table(X = 1:2, B = 2:1))
            ee(dit, dict.table(A = 1:2, B = 2:1))
            ref_rename(dit, "A", "X")

            ee(colnames(dit), c("X", "B"))
            ref_rename(dit, c("X", "B"), c("y", "z"))
            ee(colnames(dit), c("y", "z"))
            expect_error(rename(dit, "A", "b"))
        })

        test_that("rename handles multiple operations correctly",
        {
            dit <- dict.table(y = 1:2, z = 2:1)
            # Multiple renames
            expect_error(rename(dit, c("y", "y2"), c("y2", "y3")),
                         "Items of 'old' not found in names: 'y2'")
        })

        test_that("rename prevents duplicated column names",
        {
            dit <- dict.table(a = 1:2, b = 2:1)
            expect_error(
                rename(dit, "a", "b"),
                "renaming not possible due to duplicated column names: b"
            )

            hasSameColumnNames <- isTRUE(all.equal(colnames(dit), c("a", "b")))
            expect_true(hasSameColumnNames)

            expect_error(
                ref_rename(dit, "a", "b"),
                "renaming not possible due to duplicated column names: b"
            )
            hasSameColumnNames <- isTRUE(all.equal(colnames(dit), c("a", "b")))
            expect_true(hasSameColumnNames)

            dit <- dict.table(a = 1, b = 2, c = 3, d = 4)
            expect_error(
                ref_rename(dit, c("a", "b"), c("c", "d")),
                "renaming not possible due to duplicated column names: c, d"
            )
            hasSameColumnNames <- isTRUE(
                all.equal(colnames(dit), c("a", "b", "c", "d"))
            )
            expect_true(hasSameColumnNames)
        })
    })

    describe(
        "rename.default",
    {
        test_that("rename works on default objects",
        {
            v <- c(a = 1, b = 2)
            ee(rename(v, "a", "a1"), c(a1 = 1, b = 2))
            ee(rename(v, c("a", "b"), c("a1", "b1")), c(a1 = 1, b1 = 2))
        })
    })
})