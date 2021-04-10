ee = expect_equal

# ---------------
# count.Container
# ---------------
co = container("a", "b", "a", mean, mean, NULL)
ee(count(co, "a"), 2)
ee(count(co, mean), 2)
ee(count(co, "c"), 0)
ee(count(co, NA), 0)
ee(count(co, NULL), 1)
ee(count(clear(co), "a"), 0)


# ----------------
# clone.dict.table
# ----------------
s = setnew("a", "b", "a", mean, mean)
ee(count(s, "a"), 1)
ee(count(s, mean), 1)
ee(count(s, "c"), 0)
ee(count(clear(s), "a"), 0)

