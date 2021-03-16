# ---------
# Container
# ---------
co1 = container(1)
co2 = container(2)
co12 = container(1, 2)
co123 = container(1, 2, 3)
expect_true(co123 == co123)
expect_true(co1 < co12)
expect_false(co2 < co12)
expect_true(co12 < co123)

# ---
# Set
# ---
# simple sets
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

# nested sets
s = setnew(2, 3)
x = setnew(1, setnew(2, 3))
y = setnew(1, setnew(3, 2))
expect_true(x == y)
x = setnew(1, s)
y = setnew(1, s)
expect_true(x == y)

# sets with nested containers
x = setnew(1, container(2, deque(3)), 4)
y = setnew(1, container(2, deque(3)), 4)
expect_true(x == y)
expect_true(x <= y)
expect_true(x >= y)
expect_false(x < y)
expect_false(x > y)

y = setnew(container(2, deque(3)), 4, 1)
expect_true(x == y)
y = setnew(1, container(deque(3), 2), 4)
expect_false(x == y) # order in container plays a role

y = setnew(1, container(2, deque(4)), 4)
expect_true(x != y)
expect_false(x < y)
expect_false(x > y)

y = setnew(1, container(2, deque(3)))
expect_true(x > y)
expect_true(y < x)

# Put 4 also into the container
y = setnew(1, container(2, deque(3), 4))
expect_false(x > y)
expect_false(x < y)
expect_false(x == y)



# ----------
# Relational
# ----------
x = setnew(1, 2,    "1", "2")
y = setnew(   2, 3,      "2", "3")

expect_equal(x & y, setnew(2, "2"))

