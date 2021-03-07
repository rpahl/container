# ---------
# Container
# ---------
ee = expect_equal
ee(abs(container(-2, -1)), container(2, 1))
ee(exp(log(container(1, 2))), container(1, 2))
ee(cumsum(container(1, 2, 3)), container(1, 3, 6))
ee(cumsum(container(3, 2, 1)), container(3, 5, 6))

ee(exp(container(1, 2, container(3, 4))), as.container(exp(1:4)))
ee(exp(container(1, 2, list(3, 4))), as.container(exp(1:4)))
ee(exp(container(1, 2, list(container(3), 4))), as.container(exp(1:4)))
ee(exp(container(1, x = 2)), as.container(exp(c(1, x = 2))))

ee(cumsum(container(two = 2, one = 1, co = container(one = 1, two = 2))),
          container(two = 2, one = 3, co.one = 4,          co.two = 6))

# -----
# Deque
# -----
ee = expect_equal
ee(abs(deque(-1, 1)), deque(1, 1))
ee(exp(log(deque(1, 2))), deque(1, 2))
ee(cumsum(deque(1, 2, 3)), deque(1, 3, 6))
ee(cumsum(deque(3, 2, 1)), deque(3, 5, 6))

ee(exp(deque(1, 2, deque(3, 4))), as.deque(exp(1:4)))
ee(exp(deque(1, 2, list(3, 4))), as.deque(exp(1:4)))
ee(exp(deque(1, 2, list(deque(3), 4))), as.deque(exp(1:4)))

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

ee(abs(dict(a = -1, b = 2, d = dict(a = -3))),
       dict(a =  1, b = 2, d.a = 3))
ee(abs(dict(a = -1, b = 2, d = dict(x = -3))),
       dict(a =  1, b = 2, d.x = 3))

expect_error(abs(dict(d.a = -1, b = 2, d = dict(a = -3))), "duplicated keys")


# ---
# Set
# ---
ee = expect_equal
ee(abs(setnew(-1, 1)), setnew(1, 1))
ee(exp(log(setnew(1, 2))), setnew(1, 2))
ee(cumsum(setnew(1, 2, 3)), setnew(1, 3, 6))
ee(cumsum(setnew(3, 2, 1)),
          setnew(1, 3, 6)) # first sorted then calculated

ee(cumsum(setnew(two = 2, one = 1, s = setnew(one = 1, two = 2))),
          setnew(one = 1, two = 3, s.one = 4,        s.two = 6))

ee(cumsum(setnew(two = 2, one = 1, s = setnew(1, one = 1, two = 2))),
          setnew(one = 1, two = 3, s1 = 4,              s.two = 6))

