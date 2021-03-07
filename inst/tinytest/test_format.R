# ----------------
# format of values
# ----------------
ee = expect_equal
f = .format_values

ee(f(list(a = 1)), "(a = 1)")
ee(f(list(container(1))), "([1])")
ee(f(list(as.container(1:5))), "([1L, 2L, 3L, 4L, 5L])")
ee(f(list(as.container(1:6))), "(<<Container(6)>>)")

ee(f(list(deque(1))), "(|1|)")
ee(f(list(as.deque(1:5))), "(|1L, 2L, 3L, 4L, 5L|)")
ee(f(list(as.deque(1:6))), "(<<Deque(6)>>)")

ee(f(list(setnew(1))), "({1})")
ee(f(list(as.set(1:5))), "({1L, 2L, 3L, 4L, 5L})")
ee(f(list(as.set(1:6))), "(<<Set(6)>>)")

ee(f(list(dict(a = 1))), "({a = 1})")
v = 1:10
names(v) = letters[1:10]
ee(f(list(as.dict(v[1:5]))), "({a = 1L, b = 2L, c = 3L, d = 4L, e = 5L})")
ee(f(list(as.dict(v[1:6]))), "(<<Dict(6)>>)")


# -------
# formats
# -------
l = list(1, 2)
expect_equal(format.Container(l), "[1, 2]")

expect_equal(format.Deque(l), "|1, 2|")

expect_equal(format.Dict(l), "{1, 2}")

expect_equal(format.Set(l), "{1, 2}")

