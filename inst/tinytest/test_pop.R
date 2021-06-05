ee = expect_equal

# ----------
# pop_.Deque
# ----------
d = deque(1, 2, 3)
ee(pop_(d), 3)
ee(popleft_(d), 1)
ee(d, deque(2))

expect_error(pop_(deque()), "pop at empty Deque")
expect_error(popleft_(deque()), "popleft at empty Deque")


# ---------
# pop_.Dict
# ---------
d = dict(a = 1, b = 1:3)
expect_error(pop_(d, "x"), "index 'x' not found")

ee(pop_(d, "b"), 1:3)
ee(pop_(d, "a"), 1)
expect_error(pop_(d, "x"), "pop at empty Dict")


# ---------------
# pop_.dict.table
# ---------------
dit = dict.table(a = 1:3, b = 4:6)

ee(pop_(dit, "a"), 1:3)
ee(pop_(dit, 1), 4:6)
expect_error(pop_(dit, 1), "index 1 exceeds length of dict.table, which is 0")
expect_error(pop_(dit, "z"), "index 'z' not found")

