# ------------------
# peekitem.Container
# ------------------
co = container()
ee(peekitem(co), NULL)
ee(peekitem(co, default = 1), 1)
co = container(1, 2, 3)
expect_true(peekitem(co) %in% 1:3)

# ------------
# peekitem.Set
# ------------
s = as.set(co)
expect_true(peekitem(s) %in% 1:3)
ee(peekitem(setnew()), NULL)
ee(peekitem(setnew(), default = 1), 1)

# --------------
# peekitem.Deque
# --------------
d = as.deque(co)
expect_true(peekitem(d) %in% 1:3)
ee(peekitem(deque()), NULL)
ee(peekitem(deque(), default = 1), 1)

# -------------
# peekitem.Dict
# -------------
d = dict()
ee(peekitem(d), NULL)
ee(peekitem(d, default = 1), 1)
d = dict(a = 1, b = 2, c = 3)
expect_true(peekitem(d) %in% 1:3)

# -------------------
# peekitem.dict.table
# -------------------
dit = dict.table()
ee(peekitem(dit), NULL)
ee(peekitem(dit, default = 1:2), 1:2)

dit = dict.table(a = 1, b = 4)
expect_true(peekitem(dit) %in% c(1, 4))

