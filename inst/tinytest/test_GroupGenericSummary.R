# ---------
# Container
# ---------
ee = expect_equal
v = 1:4
co = as.container(v)
ee(sum(co), sum(v))
ee(prod(co), prod(v))
ee(min(co), min(v))
ee(max(co), max(v))
ee(range(co), range(v))
expect_true(is.na(sum(container(1, NA, 3))))
ee(sum(container(1, NA, 3), na.rm = TRUE), 1 + 3)
expect_true(is.na(min(container(1, NA))))
ee(min(container(1, NA), na.rm = TRUE), 1)
expect_true(all(container(T, T, T)))
expect_false(all(container(T, F, T)))
expect_true(any(container(T, F, T)))

ee(sum(container(co, co)), 2 * sum(v))
ee(range(container(1, 2, container(-10, container(10)), 3)), c(-10, 10))


# -----
# Deque
# -----
ee = expect_equal
v = 1:4
d = as.deque(v)
ee(sum(d), sum(v))
ee(prod(d), prod(v))
ee(min(d), min(v))
ee(max(d), max(v))
ee(range(d), range(v))
expect_true(is.na(sum(deque(1, NA, 3))))
ee(sum(deque(1, NA, 3), na.rm = TRUE), 1 + 3)
expect_true(is.na(min(deque(1, NA))))
ee(min(deque(1, NA), na.rm = TRUE), 1)
expect_true(all(deque(T, T, T)))
expect_false(all(deque(T, F, T)))
expect_true(any(deque(T, F, T)))

ee(sum(deque(d, d)), 2 * sum(v))
ee(range(deque(1, 2, deque(-10, deque(10)), 3)), c(-10, 10))

# ----
# Dict
# ----
ee = expect_equal
v = 1:4
names(v) = letters[1:4]
d = as.dict(v)
ee(sum(d), sum(v))
ee(prod(d), prod(v))
ee(min(d), min(v))
ee(max(d), max(v))
ee(range(d), range(v))
expect_true(is.na(sum(dict(a = 1, b = NA, c = 3))))
ee(sum(dict(a = 1, b = NA, c = 3), na.rm = TRUE), 1 + 3)
expect_true(is.na(min(dict(a = 1, b = NA))))
ee(min(dict(a = 1, b = NA), na.rm = TRUE), 1)
expect_true(all(dict(a = T, b = T, x = T)))
expect_false(all(dict(a = T, b = F, x = T)))
expect_true(any(dict(a = T, b = F, x = T)))

ee(sum(deque(d, d)), 2 * sum(v))
ee(range(dict(a = 1, b = 2, d1 = dict(x = -10, d2 = dict(y = 10)), z = 3)),
   c(-10, 10))

# ---
# Set
# ---
ee = expect_equal
v = 1:4
s = as.set(v)
ee(sum(s), sum(v))
ee(prod(s), prod(v))
ee(min(s), min(v))
ee(max(s), max(v))
ee(range(s), range(v))
expect_true(is.na(sum(setnew(1, NA, 3))))
ee(sum(setnew(1, NA, 3), na.rm = TRUE), 1 + 3)
expect_true(is.na(min(setnew(1, NA))))
ee(min(setnew(1, NA), na.rm = TRUE), 1)
expect_true(all(setnew(T, T, T)))
expect_false(all(setnew(T, F, T)))
expect_true(any(setnew(T, F, T)))

ee(sum(setnew(s, s)), sum(v))
ee(range(setnew(1, 2, setnew(-10, setnew(10)), 3)), c(-10, 10))

