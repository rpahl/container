ee = expect_equal

# ------------
# rotate.Deque
# ------------
d = deque(1, 2, 3, 4)
ee(rotate(d), deque(4, 1, 2, 3))
ee(rotate(d, 2), deque(3, 4, 1, 2))
ee(rotate(d, -1), deque(2, 3, 4, 1))

# by reference
rotate_(d)
ee(d, deque(4, 1, 2, 3))

rotate_(d, -1)
ee(d, deque(1, 2, 3, 4))

