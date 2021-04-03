# ----
# Dict
# ----
ee = expect_equal

# Key intersection
# ----------------
d0   = dict()
d1   = dict(a = 1)
d2   = dict(       b = 2)
d12  = dict(a = 1, b = 2)
d23  = dict(       b = 2, c = 3)
d1_3 = dict(a = 1,        c = 3)
d123 = dict(a = 1, b = 2, c = 3)

ee(d0 & d0, d0)
ee(d0 & d1, d0)
ee(d1 & d0, d0)
ee(d1 & d1, d1)
ee(d1 & d12, d1)
ee(d12 & d1, d1)
ee(d12 & d23, d2)
ee(d23 & d12, d2)
ee(d2 & d1_3, d0)
ee(d1_3 & d2, d0)
ee(d12 & d123, d12)
ee(d123 & d12, d12)

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
d.ref = dict(d = dict(a = 1))
ee(res, d.ref)
d1$add("b", 2)
has_used_copy_semantics <- res == d.ref
expect_true(has_used_copy_semantics)


# dicts combined with other objects
d = dict(a = 1, b = 2)
l = list(a = 3, c = 2)
ee(d & l, dict(a = 1))
ee(l & d, dict(a = 3))
ee(dict(b = 1, d = 2) & c(a = 1, b = 2, c = 3, d = 4), dict(b = 1, d = 2))

# Key union
# ---------
d0   = dict()
d1   = dict(a = 1)
d2   = dict(       b = 2)
d12  = dict(a = 1, b = 2)
d23  = dict(       b = 2, c = 3)
d1_3 = dict(a = 1,        c = 3)
d123 = dict(a = 1, b = 2, c = 3)

ee(d0 | d0, d0)
ee(d0 | d1, d1)
ee(d1 | d0, d1)
ee(d1 | d1, d1)
ee(d1 | d12, d12)
ee(d12 | d1, d12)
ee(d12 | d23, d123)
ee(d23 | d12, d123)
ee(d2 | d1_3, d123)
ee(d1_3 | d2, d123)
ee(d12 | d123, d123)

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
res = dd1 | dd2
d.ref = dict(a = 1, b = 1, d = dict(a = 1))
ee(res, d.ref)
d1$add("b", 2)
has_used_copy_semantics <- res == d.ref
expect_true(has_used_copy_semantics)


# dicts combined with other objects
d = dict(a = 1, b = 2)
l = list(a = 3, c = 2)
ee(d & l, dict(a = 1))
ee(l & d, dict(a = 3))
ee(dict(b = 1, d = 2) & c(a = 1, b = 2, c = 3, d = 4), dict(b = 1, d = 2))


# ---
# Set
# ---
ee = expect_equal

# Set intersection
# ----------------
s0   = setnew()
s1   = setnew(1)
s2   = setnew(   2)
s12  = setnew(1, 2)
s23  = setnew(   2, 3)
s1_3 = setnew(1,    3)
s123 = setnew(1, 2, 3)

ee(s0 & s0, s0)
ee(s0 & s1, s0)
ee(s1 & s0, s0)
ee(s1 & s1, s1)
ee(s1 & s12, s1)
ee(s12 & s1, s1)
ee(s12 & s23, s2)
ee(s23 & s12, s2)
ee(s2 & s1_3, s0)
ee(s1_3 & s2, s0)
ee(s12 & s123, s12)
ee(s123 & s12, s12)

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
s.ref = setnew(setnew(1))
ee(res, s.ref)
s1$add(2)
has_used_copy_semantics <- res == s.ref
expect_true(has_used_copy_semantics)


# Named elements
s1 = setnew(a = 1, b = 2)
s2 = setnew(a = 2, c = 3)
ee(s1 & s2, setnew(b = 2))
ee(s2 & s1, setnew(a = 2))

# sets combined with other objects
ee(s123 & list(1, 2), setnew(1, 2))
ee(list(1, 2) & s123, setnew(1, 2))
ee(s123 & 1:2, setnew(1, 2))
ee(1:2 & setnew(1, 2, 3), setnew(1, 2))
ee(s123 & 1:10, setnew(1, 2, 3))


# Set union
# ---------
s0   = setnew()
s1   = setnew(1)
s2   = setnew(   2)
s12  = setnew(1, 2)
s23  = setnew(   2, 3)
s1_3 = setnew(1,    3)
s123 = setnew(1, 2, 3)

ee(s0 | s0, s0)
ee(s0 | s1, s1)
ee(s1 | s0, s1)
ee(s1 | s1, s1)
ee(s1 | s12, s12)
ee(s12 | s1, s12)
ee(s12 | s23, s123)
ee(s23 | s12, s123)
ee(s2 | s1_3, s123)
ee(s1_3 | s123, s123)
ee(s12 | s123, s123)

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
res = ss1 | ss2
ee(res, setnew(setnew(1), 2))
s1$add(2)
has_used_copy_semantics <- res == setnew(setnew(1), 2)
expect_true(has_used_copy_semantics)

# Named elements
s1 = setnew(a = 1, b = 2)
s2 = setnew(a = 2, c = 3)
ee(s1 | s2, setnew(a = 1, b = 2, c = 3))
ee(s2 | s1, setnew(a = 1, a = 2, c = 3))

# sets combined with other objects
ee(setnew(1) | list(1, 2), setnew(1, 2))
ee(list(a = 1, 2) | setnew(1, 2, c = 3), setnew(a = 1, 2, c = 3))
ee(setnew(1) | as.numeric(2:3), setnew(1, 2, 3))

