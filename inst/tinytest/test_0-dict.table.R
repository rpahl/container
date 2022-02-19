ee = expect_equal
data.table = data.table::data.table

# ----------
# dict.table
# ----------
# dict.table creation works as expected
dat = data.table(A = 1, B = 2)
dit = dict.table(A = 1, B = 2)
expect_equivalent(dit, dat)

# dict.table can be initialized with or without checking of column names
dit = dict.table("x 1" = 1)
expect_equal(colnames(dit), "x 1")

dit = dict.table("x 1" = 1, check.names = TRUE)
expect_equal(colnames(dit), "x.1")

# dict.table cannot be initialized with duplicated names
expect_error(dict.table(a = 1, b = 2, a = 3),
             "duplicated keys after init: a")

# access of dict.table properities work as expected
dat = data.table(A = 1, B = 2)
dit = dict.table(A = 1, B = 2)
ee(dim(dit), dim(dat))
ee(nrow(dit), nrow(dat))
ee(ncol(dit), ncol(dat))
ee(rownames(dit), rownames(dat))
ee(colnames(dit), colnames(dat))
ee(dimnames(dit), dimnames(dat))


# dict.table rownames can be changed
dit = dict.table(A = 1:3)
ee(row.names(dit), as.character(1:3))
rownames(dit) <- letters[1:3]
ee(row.names(dit), letters[1:3])


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
expect_true(utils::hasName(add(dit, "b" = 2), "b"))


# the length of added columns must match or be of length 1
dit = dict.table(A = 1:4, B = 4:1)
expect_error(add(dit, a = 1:3),
             "Supplied 3 items to be assigned to 4 items of column 'a'")
expect_error(add(dit, a = 1:2))
expect_false(has_name(dit, "a"))
dit = add(dit, a = 1)
ee(at2(dit, "a"), rep(1, 4))


# a column cannot be added twice
dit = dict.table(A = 1:2, B = 2:1)
expect_error(add(dit, "A" = 1:2), "name 'A' exists already")


# it can be checked if a dict.table has a certain column
dit = dict.table(A = 1)
expect_true(has_name(dit, "A"))
expect_false(has_name(dit, "x"))


# a dict.table can be cleared
dit = dict.table(A = 1, B = 2)
ee(clear(dit), dict.table())


# columns can be deleted from dict.table
dit = dict.table(A = 1:2, B = 2:1, C = 3:4)
expect_true(has_name(dit, "B"))
ref_delete_at(dit, "B")
expect_false(has_name(dit, "B"))

ee(c("A", "C"), colnames(dit))
ref_delete_at(dit, "A", "C")
expect_true(is_empty(dit))


# columns can be discarded from dict.table
dit = dict.table(A = 1:2, B = 2:1, C = 3:4)
expect_true(has_name(dit, "B"))
ref_discard_at(dit, "B")
expect_false(has_name(dit, "B"))

ee(c("A", "C"), colnames(dit))
ref_discard_at(dit, c("A", "C"))
expect_true(is_empty(dit))


# columns can be discarded by index from dict.table
dit = dict.table(A = 1:2, B = 2:1, C = 3:4)
expect_true(has_name(dit, "B"))
ref_discard_at(dit, 2)
expect_false(has_name(dit, "B"))
ee(c("A", "C"), colnames(dit))

expect_true(is_empty(discard_at(dit, 1:2)))

# discarding non-existing columns works as expected
dit = dict.table(A = 1:2, B = 2:1, C = 3:4)
expect_true(is_empty(discard_at(dit, 1:10)))
expect_equal(discard_at(dit, c("X", "Y")), dit)

# deleting non-existing columns gives an error
dit = dict.table(A = 1:2, B = 2:1)
dit.copy = data.table::copy(dit)

expect_error(delete_at(dit, "X"), "column\\(s\\) not found: 'X'")
expect_error(delete_at(dit, 3), "index out of range \\(ncol = 2\\): 3")
expect_true(identical(dit, dit.copy)) # dit.copy has not changed


# dict.table can be printed
dit = dict.table(A = 1:2, B = 2:1)
out <- capture.output(print(dit))
out.expected <- c("<dict.table> with 2 rows and 2 columns",
                  "   A B",
                  "1: 1 2",
                  "2: 2 1")
ee(out, out.expected)


# dict.table has a size
dit = dict.table(A = 1:2, B = 2:1, C = 3:4)
ee(length(dit), ncol(dit))
ee(length(clear(dit)), 0)


# standard dict.table getter works as expected
dit = dict.table(A = 1:2, B = 2:1, C = 3:4)
daf = as.data.frame(dit)

ee(at2(dit, "A"), daf[["A"]])
ee(dit[["A"]], daf[["A"]])

expect_error(at2(dit, c("A", "B")), "index must be of length 1")


# peek works as expected
dit = dict.table(A = 1:2, B = 2:1)
ee(peek_at2(dit, "A"), at2(dit, "A"))
expect_error(peek_at2(dit, c("A", "B")), "index must be of length 1")

expect_true(is.null(peek_at2(dit, "X")))
ee(peek_at2(dit, "X", default = 9), rep(9, nrow(dit)))
ee(peek_at2(dit, "X", default = "a"), rep("a", nrow(dit)))


# pop works as expected
dit = dict.table(A = 1:2, B = 2:1)
ee(ref_pop(dit, "A"), 1:2)
expect_false(has_name(dit, "A"))

ee(ref_pop(dit, "B"), 2:1)
expect_true(is_empty(dit))


# columns can be renamed
dit = dict.table(A = 1:2, B = 2:1)
ref_rename(dit, "A", "X")
ee(colnames(dit), c("X", "B"))
ref_rename(dit, c("X", "B"), c("y", "z"))
ee(colnames(dit), c("y", "z"))
expect_error(rename(dit, "A", "b"), "Items of 'old' not found in names: A")


# a column can be set
dit = dict.table(A = 1:2, B = 2:1)
dit = replace_at(dit, "A", 3:4)
ee(dit[["A"]], 3:4)


# a column can only bet set if already existing unless declared to be added
dit = dict.table(A = 1:2)
expect_error(replace_at(dit, "B", 3:4), "column\\(s\\) not found: 'B'")

ref_replace_at(dit, "B", 5:6, .add = TRUE)
ref_replace_at(dit, "C" = 7:8, .add = TRUE)
ee(dit[["B"]], 5:6)
ee(colnames(dit), c("A", "B", "C"))

expect_error(replace_at(dit, 4, 1:2, .add = TRUE),
             "index out of range \\(ncol = 3\\): 4")


# the length of set column must match or be of length 1
dit = dict.table(A = 1:4, B = 4:1)
expect_error(replace_at(dit, "A", 1:3),
             "Supplied 3 items to be assigned to 4 items of column 'A'")
expect_error(replace_at(dit, "A", 1:2))
ee(dit[["A"]], 1:4)

ee(replace_at(dit, "A", 9)[["A"]], rep(9, 4))


# a column can be set by numeric index
dit = dict.table(A = 1:2, B = 2:1)
ee(replace_at(dit, 2, 0)[[2]], rep(0, 2))

# rbind works as expected for dict.tables
dat = data.table(A = 1:2, B = 2:1)
dit = dict.table(dat)
dat2 = rbind(dat, dat)
dit2 = rbind(dit, dit)
expect_true(is.dict.table(dit2))
ee(data.table::as.data.table(dit2), dat2)
dit.dat = rbind(dit, dat)
expect_true(is.dict.table(dit.dat))
ee(data.table::as.data.table(dit.dat), dat2)

# cbind works as expected for dict.tables
expect_error(cbind(dit, dit), "found duplicated column names: A, B")
ee(cbind(dit[, 1], dit[, 2]), dit)

# cbind works with data.tables
ee(cbind(dict.table(a = 1:2), data.table(b = 1:2)),
   dict.table(a = 1:2, b = 1:2))

ee(cbind(data.table(a = 1:2), dict.table(b = 1:2)),
   data.table(a = 1:2, b = 1:2))

# dict.table coercion works as expected
dit = dict.table(A = 1:2, B = 3:4)
expect_true(is.data.frame(as.data.frame(dit)))

dat = data.table::as.data.table(dit)
expect_true(data.table::is.data.table(dat))
ee(dat, data.table(A = 1:2, B = 3:4))
expect_true(is.dict.table(dit))
data.table::set(dat, j = "C", value = 1:2)
expect_false(has_name(dit, "C"))

daf = as.data.frame(dit)
ee(daf, data.frame(A = 1:2, B = 3:4))
expect_true(is.dict.table(dit))

