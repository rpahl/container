# ----------------
# format of values
# ----------------
ee = expect_equal
f = function(...) .format_values(..., left = "", right ="")

ee(f(list(a = 1)), "a = 1")
ee(f(list(container(1))), "[1]")
ee(f(list(as.container(1:5))), "[1L, 2L, 3L, 4L, 5L]")
ee(f(list(as.container(1:6))), "<<Container(6)>>")
ee(f(list(as.container(1:10))), "<<Container(10)>>")
ee(f(list(as.container(1:10)), limit = 20), "[1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L]")

ee(f(list(deque(1))), "|1|")
ee(f(list(as.deque(1:5))), "|1L, 2L, 3L, 4L, 5L|")
ee(f(list(as.deque(1:6))), "<<Deque(6)>>")

ee(f(list(setnew(1))), "{1}")
ee(f(list(as.set(1:5))), "{1L, 2L, 3L, 4L, 5L}")
ee(f(list(as.set(1:6))), "<<Set(6)>>")

ee(f(list(dict(a = 1))), "{a = 1}")
v = 1:10
names(v) = letters[1:10]
ee(f(list(as.dict(v[1:5]))), "{a = 1L, b = 2L, c = 3L, d = 4L, e = 5L}")
ee(f(list(as.dict(v[1:6]))), "<<Dict(6)>>")

ee(f(container()), "")
ee(f(container(list( ))), "list()")
ee(f(container(list(1))), "<<list(1)>>")
ee(f(container(list(1)), limit = 1), "list(1)")
ee(f(container(list(1, 2)), limit = 1), "<<list(2)>>")
ee(f(container(list(1, 2)), limit = 2), "list(1, 2)")

ee(f(container(numeric( ))), "numeric()")
ee(f(container(numeric(0))), "numeric()")
ee(f(container(c(1))), "1")
ee(f(container(c(1)), limit = 1), "1")
ee(f(container(c(1, 2)), limit = 1), "<<numeric(2)>>")
ee(f(container(c(1, 2)), limit = 2), "(1, 2)")

ee(f(container(c("a"))), '"a"')
ee(f(container(c("a")), limit = 1), '"a"')
ee(f(container(c("a", "b")), limit = 1), "<<character(2)>>")
ee(f(container(c("a", "b")), limit = 2), '("a", "b")')

# -------
# formats
# -------
l = list(1, 2)
ee = expect_equal
ee(format.Container(1), "[1]")
ee(format.Deque(1), "|1|")
ee(format.Dict(1), "{1}")
ee(format.Set(1), "{1}")



