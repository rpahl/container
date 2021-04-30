ee = expect_equal

# ----------
# peek.Deque
# ----------
d = deque(1, 2, 3)
ee(peek(d), 3)
ee(peekleft(d), 1)
ee(peek(deque()), NULL)
ee(peekleft(deque()), NULL)
ee(peek(deque(), default = 1), 1)
ee(peekleft(deque(), default = 0), 0)


# ---------
# peek.Dict
# ---------
d = dict(a = 1, b = 1:3)
ee(peek(d, "b"), 1:3)
ee(peek(d, "x"), NULL)
ee(peek(d, "x", default = 4:6), 4:6)


# ---------------
# peek.dict.table
# ---------------
dit = dict.table(a = 1:3, b = 4:6)
ee(peek(dit, "a"), getval(dit, "a"))
ee(peek(dit, 1), getval(dit, 1))
ee(peek(dit, 3), NULL)
ee(peek(dit, "x"), NULL)
ee(peek(dit, "x", default = 0), rep(0, 3))

