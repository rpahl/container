ip <- names(installed.packages()[, "Package"])
hasDT <- "data.table" %in% ip
if (!hasDT){
    exit_file("skip dict.table, because data.table pkg is not installed")
}

data.table <- data.table::data.table
as.data.table <- data.table::as.data.table

# dict.table creation works as expected
dat = data.table(A = 1, B = 2)
dit = dict.table(A = 1, B = 2)
expect_equivalent(dit, dat)


# access of dict.table properities work as expected
dat = data.table(A = 1, B = 2)
dit = dict.table(A = 1, B = 2)
expect_equal(dim(dit), dim(dat))
expect_equal(nrow(dit), nrow(dat))
expect_equal(ncol(dit), ncol(dat))
expect_equal(rownames(dit), rownames(dat))
expect_equal(colnames(dit), colnames(dat))
expect_equal(dimnames(dit), dimnames(dat))


# dict.table rownames can be changed
dit = dict.table(A = 1:3)
expect_equal(row.names(dit), as.character(1:3))
rownames(dit) <- letters[1:3]
expect_equal(row.names(dit), letters[1:3])


# dict.table is created as copy of another data.table
dat <- data.table(A = 1)
dat2 <- data.table::copy(dat)
dit = dict.table(dat)

dat[1, 1] <- 9
expect_false(dat == dat2)
expect_true(dit == dat2)


# a column can be addded to a dict.table if name is specified as character
dit = dict.table(A = 1)
expect_false("x" %in% colnames(dit))
add(dit, "x", 2)
expect_true("x" %in% colnames(dit))

expect_error(add(dit, 3, 3),
             "Item 1 of column numbers in j is 3 which is outside range")


# the length of added columns must match or be of length 1
dit = dict.table(A = 1:4, B = 4:1)
expect_error(add(dit, "x", 1:3), "Supplied 3 items to be assigned to 4 items of column 'x'")
expect_error(add(dit, "x", 1:2))
expect_false(has(dit, "x"))
add(dit, "x", 1)
expect_equal(getval(dit, "x"), rep(1, 4))


# a column cannot be added twice
dit = dict.table(A = 1:2, B = 2:1)
expect_error(add(dit, "A", 1:2), "column 'A' already in dict.table")


# it can be checked if a dict.table has a certain column
dit = dict.table(A = 1)
expect_true(has(dit, "A"))
expect_false(has(dit, "x"))


# a dict.table can be cleared
dit = dict.table(A = 1, B = 2)
expect_equal(clear(dit), dict.table())


# columns can be deleted from dict.table
dit = dict.table(A = 1:2, B = 2:1, C = 3:4)
expect_true(has(dit, "B"))
delete(dit, "B")
expect_false(has(dit, "B"))

expect_equal(c("A", "C"), colnames(dit))
delete(dit, c("A", "C"))
expect_true(empty(dit))


# columns can be discarded from dict.table
dit = dict.table(A = 1:2, B = 2:1, C = 3:4)
expect_true(has(dit, "B"))
discard(dit, "B")
expect_false(has(dit, "B"))

expect_equal(c("A", "C"), colnames(dit))
discard(dit, c("A", "C"))
expect_true(empty(dit))


# columns can be discarded by index from dict.table
dit = dict.table(A = 1:2, B = 2:1, C = 3:4)
expect_true(has(dit, "B"))
discard(dit, 2)
expect_false(has(dit, "B"))
expect_equal(c("A", "C"), colnames(dit))

expect_silent(discard(dit, 1:2))
expect_true(empty(dit))


# discarding non-existing columns works as expected
dit = dict.table(A = 1:2, B = 2:1, C = 3:4)
expect_silent(discard(dit, 1:10))
expect_true(empty(dit))
expect_silent(discard(dit, c("X", "Y")))


# deleting non-existing columns gives an error
dit = dict.table(A = 1:2, B = 2:1)
dit.copy = data.table::copy(dit)

expect_error(delete(dit, "X"), "Column 'X' not in dict.table")
expect_error(delete(dit, 3), "Column '3' out of range")
expect_true(identical(dit, dit.copy)) # dit.copy has not changed


# if delete operation fails, dict.table object remains untouched
dit = dict.table(A = 1:2, B = 2:1)
dit.copy = data.table::copy(dit)

expect_error(delete(dit, c("B", "X")), "Column 'X' not in dict.table")
expect_equal(dit, dit.copy)


# dict.table can be printed
dit = dict.table(A = 1:2, B = 2:1)
out <- capture.output(print(dit))
out.expected <- c("<dict.table> with 2 rows and 2 columns",
                  "   A B",
                  "1: 1 2",
                  "2: 2 1")
expect_equal(out, out.expected)


# dict.table has a size
dit = dict.table(A = 1:2, B = 2:1, C = 3:4)
expect_equal(length(dit), ncol(dit))
expect_equal(length(clear(dit)), 0)


# standard dict getter works as expected
dit = dict.table(A = 1:2, B = 2:1, C = 3:4)
daf = as.data.frame(dit)

expect_equal(getval(dit, "A"), daf[["A"]])
expect_error(getval(dit, c("A", "B")), "column index must be of length 1")


# peek works as expected
dit = dict.table(A = 1:2, B = 2:1)
expect_equal(peek(dit, "A"), getval(dit, "A"))
expect_error(peek(dit, c("A", "B")), "column index must be of length 1")

expect_true(is.null(peek(dit, "X")))
expect_equal(peek(dit, "X", default = 9), rep(9, nrow(dit)))
expect_equal(peek(dit, "X", default = "a"), rep("a", nrow(dit)))


# pop works as expected
dit = dict.table(A = 1:2, B = 2:1)
expect_equal(pop(dit, "A"), 1:2)
expect_false(has(dit, "A"))

expect_equal(pop(dit, "B"), 2:1)
expect_true(empty(dit))


# popitem works as expected
set.seed(123)
dit = dict.table(A = 1:2, B = 2:1)
expect_equal(popitem(dit), 1:2)
expect_false(has(dit, "A"))

expect_equal(popitem(dit), 2:1)
expect_true(empty(dit))


# columns can be renamed
dit = dict.table(A = 1:2, B = 2:1)
rename(dit, "A", "X")
expect_equal(colnames(dit), c("X", "B"))
rename(dit, c("X", "B"), c("y", "z"))
expect_equal(colnames(dit), c("y", "z"))
expect_error(rename(dit, "A", "b"))


# a column can be set
dit = dict.table(A = 1:2, B = 2:1)
setval(dit, "A", 3:4)
expect_equal(getval(dit, "A"), 3:4)


# a column can only bet set if already existing unless declared to be added
dit = dict.table(A = 1)
expect_error(setval(dit, "B", 1), "column 'B' not in dict.table")

setval(dit, "B", 1, add = TRUE)
expect_equal(getval(dit, "B"), 1)
expect_equal(colnames(dit), c("A", "B"))

expect_error(setval(dit, 3, 1, add = TRUE),
             "Item 1 of column numbers in j is 3 which is outside range")


# the length of set column must match or be of length 1
dit = dict.table(A = 1:4, B = 4:1)
expect_error(setval(dit, "A", 1:3),
             "Supplied 3 items to be assigned to 4 items of column 'A'")
expect_error(setval(dit, "A", 1:2))
expect_equal(getval(dit, "A"), 1:4)

setval(dit, "A", 9)
expect_equal(getval(dit, "A"), rep(9, 4))


# a column can be set by numeric index
dit = dict.table(A = 1:2, B = 2:1)
setval(dit, 2, 0)
expect_equal(getval(dit, 2), rep(0, 2))
expect_error(setval(dit, 3, 1:2), "3 is outside range")


# column names can be sorted increasingly and decreasingly
dit = dict.table(A = 1:2, B = 2:1)
daf = as.data.frame(dit)
expect_equal(colnames(dit), colnames(daf))
sortkey(dit)
expect_equal(colnames(dit), colnames(daf))
sortkey(dit, decr = TRUE)
expect_equal(colnames(dit), rev(colnames(daf)))


# rbind works as expected for dict.tables
dat = data.table(A = 1:2, B = 2:1)
dit = dict.table(dat)
dat2 <- rbind(dat, dat)
dit2 <- rbind(dit, dit)
expect_true(is.dict.table(dit2))
expect_equal(as.data.table(dit2), dat2)
dit.dat <- rbind(dit, dat)
expect_true(is.dict.table(dit.dat))
expect_equal(as.data.table(dit.dat), dat2)


# dict.table coercion works as expected
dit = dict.table(A = 1:2, B = 3:4)
expect_true(is.data.frame(as.data.frame(dit)))

dat = as.data.table(dit)
expect_true(is.data.table(dat))
expect_equal(dat, data.table(A = 1:2, B = 3:4))
expect_true(is.dict.table(dit))
data.table::set(dat, j = "C", value = 1:2)
expect_false(has(dit, "C"))

daf = as.data.frame(dit)
expect_equal(daf, data.frame(A = 1:2, B = 3:4))
expect_true(is.dict.table(dit))

# By reference means dit also becomes a data.table
dat <- as.data.table(dit, copy = FALSE)
expect_true(is.data.table(dat))
expect_false(is.dict.table(dit))
expect_true(is.data.table(dit))
data.table::set(dat, j = "C", value = 1:2)
expect_equal(dat, dit)


