# ----------
# Comparison
# ----------
x = setnew(1, 2,    "1", "2")
y = setnew(   2, 3,      "2", "3")
expect_true(x > setnew())
expect_true(setnew() < x)
expect_true(x != setnew())
expect_false(x == setnew())
expect_false(x < x)
expect_false(x > x)
expect_true(x == x)
expect_false(x != x)
expect_true(x <= x)
expect_true(x >= x)

expect_true(x != y)
expect_true(y != x)
expect_false(x < y)
expect_false(x > y)
expect_false(y < x)
expect_false(y > x)
expect_false(x <= y)
expect_false(x >= y)
expect_false(y <= x)
expect_false(y >= x)

x = setnew(1)
y = setnew(1, "1")
expect_true(x != y)

x = setnew(1)
y = setnew(1, 2)
expect_true(x < y)
expect_true(x <= y)
expect_true(y > x)
expect_true(y >= y)

x = setnew(1, setnew(2, 3))
y = setnew(1, setnew(3, 2))
expect_true(x == y)

