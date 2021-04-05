# ---------
# container
# ---------
co1 = container(1)
co12 = container(1, 2)
coco = container(3, co12, 4)

expect_false(all.equal(co1, 1))
expect_false(all.equal(co1, list(1)))
expect_false(all.equal(co12, coco))
expect_true(all.equal(coco, coco))

c1 = container(1, container(2, v = 1))
c2 = container(1, container(2, v = 1))
expect_true(all.equal(c1, c2))
c2b = container(1, container(2, v = 2))
expect_false(isTRUE(all.equal(c1, c2b)))

c1 = container(1, container(2, container(3, data.frame(a = 1, b = 2))))
c2 = container(1, container(2, container(3, data.frame(a = 1, b = 2))))
expect_true(all.equal(c1, c2))
c2b = container(1, container(2, container(3, data.frame(a = 1, b = 3))))
expect_false(isTRUE(all.equal(c1, c2b)))


# -----
# mixed
# -----
expect_true(all.equal(dict(a = 1), dict(a = 1)))
expect_false(isTRUE(all.equal(dict(a = 1), dict(b = 1))))

d1 = dict(a = 1, b = setnew(1, 2), c = container(3, 4))
dd = dict(a = 1, b = setnew(1, 2), c = container(3, 4))
expect_true(all.equal(d1, dd))
dd = dict(a = 1, b = setnew(2, 1), c = container(3, 4))
expect_true(all.equal(d1, dd))
dd = dict(a = 1, b = setnew(2, 1), c = container(3, x = 4))
expect_false(isTRUE(all.equal(d1, dd)))

d1 = dict(a = 1, c = container(3, y = setnew(deque(1, 2), deque(3, 4))))
dd = dict(a = 1, c = container(3, y = setnew(deque(1, 2), deque(3, 4))))
expect_true(all.equal(d1, dd))
dd = dict(a = 1, c = container(3, y = setnew(deque(3, 4), deque(1, 2))))
#expect_true(all.equal(d1, dd)) # TODO: fails
dd = dict(a = 1, c = container(3, y = setnew(deque(1, 2), deque(4, 3))))
expect_false(isTRUE(all.equal(d1, dd)))

# Constructing with copy semantics prevents side effects on the comparison
co1 = container(1)
d1 = dict(a = 1, c = container(3, y = setnew(deque(1, 2), co1)))
dd = dict(a = 1, c = container(3, y = setnew(deque(1, 2), container(1))))
expect_true(all.equal(d1, dd))
co1$clear()
expect_true(all.equal(d1, dd))

# In contrast, reference semantics yield side effects
co1 = container(1)
d1 = Dict$new(a = 1, c = co1)
dd = Dict$new(a = 1, c = container(1))
expect_true(all.equal(d1, dd))
co1$clear()
expect_false(isTRUE(all.equal(d1, dd)))

