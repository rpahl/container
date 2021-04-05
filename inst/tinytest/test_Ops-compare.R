# ---------
# Container
# ---------

# Simple elements
co1 = container(1)
co2 = container(2)
co3 = container(3)
co12 = container(1, 2)
co21 = container(2, 1)
co123 = container(1, 2, 3)
expect_error(container(1, 2, 3) < 1:4, "both arguments must be iterable")

expect_false(co123 < co123)
expect_false(co123 > co123)
expect_true(co123 <= co123)
expect_true(co123 >= co123)
expect_true(co123 == co123)
expect_true(co1 != co12)
expect_true(co1 < co12)
expect_false(co2 < co12)
expect_true(co12 < co123)
expect_true(co12 < co2)

# Nested containers
coco0 = container(0, co3)
coco1 = container(1, co12)
coco2 = container(1, co21)

expect_true(coco1 < coco2)
expect_true(coco2 > coco1)
expect_true(coco1 != coco2)
expect_true(coco0 < coco1)
expect_true(container(1, co12) < container(1, co123))
expect_false(container(1, co123) < container(1, co123))
expect_true(container(1, co123) <= container(1, co123))

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
expect_true(x < y)

expect_false(x > y)
expect_false(y < x)
expect_true(y > x)
expect_true(x <= y)
expect_false(x >= y)
expect_false(y <= x)
expect_true(y >= x)

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
expect_true(x < y)

y = setnew(1, container(2, deque(3)))
expect_false(x == y)
expect_error(expect_true(x > y))



# ----------
# Relational
# ----------
s0   = setnew()
s1   = setnew(1)
s12  = setnew(1, 2)
s23  = setnew(   2, 3)
s1_3 = setnew(1,    3)
s123 = setnew(1, 2, 3)

expect_equal(s0 & s0, s0) # TODO:

original_sets_were_not_altered =
    isTRUE(all.equal(s0,   setnew()))           &&
    isTRUE(all.equal(s1,   setnew(1)))          &&
    isTRUE(all.equal(s12,  setnew(1, 2)))       &&
    isTRUE(all.equal(s23,  setnew(   2, 3)))    &&
    isTRUE(all.equal(s1_3, setnew(1,   3)))     &&
    isTRUE(all.equal(s123, setnew(1, 2, 3)))

expect_true(original_sets_were_not_altered)

