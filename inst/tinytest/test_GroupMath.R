# ---------
# Container
# ---------
ee = expect_equal
ee(abs(container(-2, -1)), container(2, 1))
ee(exp(log(container(1, 2))), container(1, 2))
ee(cumsum(container(1, 2, 3)), container(1, 3, 6))
ee(cumsum(container(3, 2, 1)), container(3, 5, 6))

# -----
# Deque
# -----
ee = expect_equal
ee(abs(deque(-1, 1)), deque(1, 1))
ee(exp(log(deque(1, 2))), deque(1, 2))
ee(cumsum(deque(1, 2, 3)), deque(1, 3, 6))
ee(cumsum(deque(3, 2, 1)), deque(3, 5, 6))

# ----
# Dict
# ----
ee = expect_equal
ee(abs(dict(b = -1, a = -2)), dict(a = 2, b = 1))
ee(exp(log(dict(x = 1, b = 2))), dict(b = 2, x = 1))
ee(cumsum(dict(a = 1, b = 2, c = 3)),
          dict(a = 1, b = 3, c = 6))

ee(cumsum(dict(c = 2, b = 1, a = 3)),
          dict(a = 3, b = 4, c = 6)) # first sorted then calculated

# ---
# Set
# ---
ee = expect_equal
ee(abs(setnew(-1, 1)), setnew(1, 1))
ee(exp(log(setnew(1, 2))), setnew(1, 2))
ee(cumsum(setnew(1, 2, 3)), setnew(1, 3, 6))
ee(cumsum(setnew(3, 2, 1)),
          setnew(1, 3, 6)) # first sorted then calculated

