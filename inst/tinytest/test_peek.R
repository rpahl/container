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

# --------
# peekitem
# --------
co = container()
ee(peekitem(co), NULL)
ee(peekitem(co, default = 1), 1)
co = container(1, 2, 3)
expect_true(peekitem(co) %in% 1:3)

s = as.set(co)
expect_true(peekitem(s) %in% 1:3)
ee(peekitem(setnew()), NULL)
ee(peekitem(setnew(), default = 1), 1)

d = as.deque(co)
expect_true(peekitem(d) %in% 1:3)
ee(peekitem(deque()), NULL)
ee(peekitem(deque(), default = 1), 1)

d = dict()
ee(peekitem(d), NULL)
ee(peekitem(d, default = 1), 1)
d = dict(a = 1, b = 2, c = 3)
expect_true(peekitem(d) %in% 1:3)

dit = dict.table()
ee(peekitem(dit), NULL)
ee(peekitem(dit, default = 1:2), 1:2)

dit = dict.table(a = 1, b = 4)
expect_true(peekitem(dit) %in% c(1, 4))

