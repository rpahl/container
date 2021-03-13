f... = get_label
f = function(...) get_label(..., useDots = FALSE)
ee = expect_equal

# -------------
# Special cases
# -------------
expect_error(f...())
expect_error(f())
ee(f(NA), "NA")
ee(f(NULL), "NULL")

ee(f(raw()), "<<raw>>")
ee(f(raw(5)), "<<raw>>")

ee(f(mean), "<<function>>") # TODO: improve
ee(f(getClass("MethodDefinition")), "<<classRepresentation>>")
ee(f(lm(y ~ x, data = data.frame(y = 1:2, x = 1:2))), "<<lm>>")

ee(f(new.env()), "<<environment>>")
ee(f(Sys.time()), "<<POSIXct>>")



# -------
# numeric
# -------
ee(f...(as.numeric(NA)), "NA")
ee(f(as.numeric(NA)), "NA")
ee(f...(numeric()), "numeric()")
ee(f...(double()), "numeric()")
ee(f...(numeric(1)), "0")
ee(f...(numeric(4)), "(0 0 0 0)")
ee(f...(numeric(5)), "(0 0 0 0 ...)")
ee(f...(numeric(5), vec.len = 5), "(0 0 0 0 0)")
ee(f...(numeric(3), vec.len = 1), "(0 ...)")
ee(f(numeric(4)), f...(numeric(4)))
ee(f(numeric(5)), "<<numeric(5)>>")
ee(f(numeric(5), vec.len = 1), "<<numeric(5)>>")
ee(f...(1), "1")
ee(f...(1:2/2), "(0.5 1.0)")


# ------
# factor
# ------
ee(f...(as.factor(NA)), "NA")
ee(f(as.factor(NA)), "NA")
ee(f...(factor()), "factor()")
ee(f...(factor(1)), "1")
ee(f...(factor(4)), "4")
ee(f...(factor(1:4)), "(1 2 3 4)")
ee(f...(factor(1:5)), "(1 2 3 4 ...)")
ee(f...(factor(1:5), vec.len = 5), "(1 2 3 4 5)")
ee(f...(factor(1:3), vec.len = 1), "(1 ...)")
ee(f(factor(1:4)), f...(factor(1:4)))
ee(f(factor(1:5)), "<<factor(5)>>")
ee(f(factor(1:2), vec.len = 1), "<<factor(2)>>")


# -------
# integer
# -------
ee(f...(as.integer(NA)), "NA")
ee(f(as.integer(NA)), "NA")
ee(f...(integer()), "integer()")
ee(f...(integer(1)), "0L")
ee(f...(integer(4)), "(0L 0L 0L 0L)")
ee(f...(integer(5)), "(0L 0L 0L 0L ...)")
ee(f...(integer(5), vec.len = 5), "(0L 0L 0L 0L 0L)")
ee(f...(integer(3), vec.len = 1), "(0L ...)")
ee(f(integer(4)), f...(integer(4)))
ee(f(integer(5)), "<<integer(5)>>")
ee(f(integer(2), vec.len = 1), "<<integer(2)>>")
ee(f...(1:100), "(1L 2L 3L 4L ...)")
ee(f(1:100), "<<integer(100)>>")
ee(f...(1:3, markInteger = FALSE), "(1 2 3)")
ee(f...(1:10, markInteger = FALSE), "(1 2 3 4 ...)")


# -------
# logical
# -------
ee(f...(as.logical(NA)), "NA")
ee(f(as.logical(NA)), "NA")
ee(f...(logical()), "logical()")
ee(f...(logical(1)), "FALSE")
ee(f...(logical(3)), "(FALSE FALSE ...)")
ee(f...(logical(3), vec.len = 5), "(FALSE FALSE ...)")
ee(f...(logical(3), vec.len = 6), "(FALSE FALSE FALSE)")
ee(f...(logical(4), vec.len = 6), "(FALSE FALSE FALSE ...)")

ee(f...(logical(3), vec.len = 1), "(FALSE ...)")
ee(f(logical(2)), f...(logical(2)))
ee(f(logical(5)), "<<logical(5)>>")
ee(f(logical(2), vec.len = 1), "<<logical(2)>>")


# ---------
# character
# ---------
ee(f...(as.character(NA)), "NA")
ee(f(as.character(NA)), "NA")
ee(f...(character()), "character()")
ee(f...(character(1)), '""')
ee(f...(character(4)), '("" "" "" "")')
ee(f...(character(5)), '("" "" "" "" ...)')
ee(f...(character(5), vec.len = 5), '("" "" "" "" "")')
ee(f...(character(3), vec.len = 1), '("" ...)')
ee(f(character(4)), f...(character(4)))
ee(f(character(5)), "<<character(5)>>")
ee(f(character(2), vec.len = 1), "<<character(2)>>")


# ----
# list
# ----
ee(f(list()), "list()")
ee(f(list(1)), "list(1)")
ee(f(list(NULL)), "list(1)")

# ---------
# Container
# ---------
ee(f(container()), "Container")

# -----------
# matrix & co
# -----------
ee(f(matrix()), "<<matrix(1x1)>>")
ee(f(matrix(1:4)), "<<matrix(4x1)>>")
ee(f(matrix(1:4, nrow = 2)), "<<matrix(2x2)>>")
ee(f(matrix(1:10, nrow = 5)), "<<matrix(5x2)>>")

ee(f(data.frame()), "<<data.frame(0x0)>>")
ee(f(data.frame(a = 1:3)), "<<data.frame(3x1)>>")
ee(f(data.frame(a = 1:2, b = 3:4)), "<<data.frame(2x2)>>")

