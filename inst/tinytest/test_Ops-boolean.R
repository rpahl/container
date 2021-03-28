# ----
# Dict
# ----

# &-Operator
d0   = dict()
d1   = dict(a = 1)
d2   = dict(       b = 2)
d12  = dict(a = 1, b = 2)
d23  = dict(       b = 2, c = 3)
d1_3 = dict(a = 1,        c = 3)
d123 = dict(a = 1, b = 2, c = 3)

expect_equal(d0 & d0, d0)
expect_equal(d0 & d1, d0)
expect_equal(d1 & d0, d0)
expect_equal(d1 & d1, d1)
expect_equal(d1 & d12, d1)
expect_equal(d12 & d1, d1)
expect_equal(d12 & d23, d2)
expect_equal(d23 & d12, d2)
expect_equal(d2 & d1_3, d0)
expect_equal(d1_3 & d2, d0)
expect_equal(d12 & d123, d12)
expect_equal(d123 & d12, d12)

original_dicts_were_not_altered =
    isTRUE(all.equal(d0,   dict()))                 &&
    isTRUE(all.equal(d1,   dict(a=1)))              &&
    isTRUE(all.equal(d2,   dict(     b=2)))         &&
    isTRUE(all.equal(d12,  dict(a=1, b=2)))         &&
    isTRUE(all.equal(d23,  dict(     b=2, c=3)))    &&
    isTRUE(all.equal(d1_3, dict(a=1,      c=3)))    &&
    isTRUE(all.equal(d123, dict(a=1, b=2, c=3)))

expect_true(original_dicts_were_not_altered)

# Nested dicts
d1  = dict(a = 1)
dd1 = dict(a = 1, d = d1)
dd2 = dict(b = 1, d = d1)
res = dd1 & dd2
expect_equal(res, dict(d = dict(a = 1)))
d1$add("b", 2)
expect_equal(res, dict(d = dict(a = 1)))


# ---
# Set
# ---

# &-Operator
s0   = setnew()
s1   = setnew(1)
s2   = setnew(   2)
s12  = setnew(1, 2)
s23  = setnew(   2, 3)
s1_3 = setnew(1,    3)
s123 = setnew(1, 2, 3)

expect_equal(s0 & s0, s0)
expect_equal(s0 & s1, s0)
expect_equal(s1 & s0, s0)
expect_equal(s1 & s1, s1)
expect_equal(s1 & s12, s1)
expect_equal(s12 & s1, s1)
expect_equal(s12 & s23, s2)
expect_equal(s23 & s12, s2)
expect_equal(s2 & s1_3, s0)
expect_equal(s1_3 & s2, s0)
expect_equal(s12 & s123, s12)
expect_equal(s123 & s12, s12)

original_sets_were_not_altered =
    isTRUE(all.equal(s0,   setnew()))           &&
    isTRUE(all.equal(s1,   setnew(1)))          &&
    isTRUE(all.equal(s2,   setnew(2)))          &&
    isTRUE(all.equal(s12,  setnew(1, 2)))       &&
    isTRUE(all.equal(s23,  setnew(   2, 3)))    &&
    isTRUE(all.equal(s1_3, setnew(1,   3)))     &&
    isTRUE(all.equal(s123, setnew(1, 2, 3)))

expect_true(original_sets_were_not_altered)

# Nested sets
s1  = setnew(1)
ss1 = setnew(1, s1)
ss2 = setnew(2, s1)
res = ss1 & ss2
expect_equal(res, setnew(setnew(1)))
s1$add(2)
expect_equal(res, setnew(setnew(1)))

