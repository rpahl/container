ee = expect_equal


d = deque(1, 2, 3)
ee(peek(d), 3)
ee(peekleft(d), 1)
ee(peek(deque()), NULL)
ee(peekleft(deque()), NULL)
ee(peek(deque(), default = 1), 1)
ee(peekleft(deque(), default = 0), 0)
ee(peek_at2(d), NULL)
ee(peek_at2(d, default = 1), 1)


