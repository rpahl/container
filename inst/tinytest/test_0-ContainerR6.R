ee = expect_equal

# ----------
# initialize
# ----------
co <- Container$new()
ee(attr(co, "class"), c("Container", "Iterable", "R6"))
ee(mode(co$values()), "list")

co <- Container$new(environment())
ee(co$length(), 1)

co <- Container$new(environment(), foo = identity)
ee(co$length(), 2)

# initialized names are kept
co <- Container$new(A = 1, B = 2)
ee(names(co$values()), c("A", "B"))


# ---
# add
# ---
co <- Container$new()
expect_true(co$is_empty())
co$add(1)
ee(co$values(), list(1))

# NULL and empty lists can be added to a Container
co <- Container$new()
co$add(NULL)
co$add(list())
co$add(0)
co$add(NULL)
co$add(list())
ee(co$values(), list(NULL, list(), 0, NULL, list()))

# non-trivial objects are added correctly
v <- 1:10
env <- new.env()
ll <- list(1, 2, "A")
foo <- function() print("foo")
collection <- c(list(v), list(env), list(ll), list(foo))

co <- Container$new()
co$add(v)$add(env)$add(ll)$add(foo)
ee(co$values(), collection)
ee(co$length(), length(collection))

# a Container can be added to a Container
co <- Container$new(1, 2)
coco <- Container$new()
coco$add(co)
ee(coco$values()[[1]], co)
ee(coco$values()[[1]]$values(), list(1, 2))

# named elements can be added to a Container
co <- Container$new()
co$add(a = 1, 2, b = 3)
ee(co$values(), list(a = 1, 2, b = 3))


# --
# at
# --
co = Container$new(a = 1, 2, b = 3, 4)
ee(co$at(1), Container$new(a = 1))
ee(co$at(2), Container$new(2))
ee(co$at(c("a", "b")), Container$new(a = 1, b = 3))
ee(co$at(list(1, "b")), Container$new(a = 1, b = 3))

ee(co$at(1:2), Container$new(a = 1, 2))
ee(co$at("a"), co$at(match("a", names(co))))
ee(co$at("b"), co$at(match("b", names(co))))

ee(co$at(c(1, 1)), Container$new(a = 1, a = 1))
ee(co$at(c("a", "a")), Container$new(a = 1, a = 1))
ee(co$at(NULL), Container$new())

expect_error(co$at(), "'index' is missing")
expect_error(co$at(0), "index must be > 0")
expect_error(co$at(-1), "index must be > 0")
expect_error(co$at("c"), "index 'c' not found")
expect_error(co$at(as.numeric(NA)), "index must not be 'NA'")


# ---
# at2
# ---
co = Container$new(a = 1, 2, b = 3, 4)
ee(co$at2(1), 1)
ee(co$at2(2), 2)
ee(co$at2("a"), 1)
expect_error(co$at2(NULL), "index must be of length 1")
expect_error(co$at2(), "'index' is missing")
expect_error(co$at2(1:2), "index must be of length 1")
expect_error(co$at2(0), "index must be > 0")
expect_error(co$at2(-1), "index must be > 0")
expect_error(co$at2(5), "index 5 exceeds length of Container, which is 4")
expect_error(co$at2(as.numeric(NA)), "index must not be 'NA'")
expect_error(co$at2(c("a", "b")), "index must be of length 1")
expect_error(co$at2("c"), "index 'c' not found")



# -----
# clear
# -----
ee(Container$new()$clear(), Container$new())
ee(Container$new(1, 2)$clear(), Container$new())
ee(Container$new(a = 1, b = 2)$clear(), Container$new())

# -----
# count
# -----
# number of element occurrences can be counted
co <- Container$new("a", "a", "b", "a", "c")
ee(co$count("a"), 3)
ee(co$count("b"), 1)
ee(co$count("c"), 1)
ee(co$count("d"), 0)

# ------
# delete
# ------
# elements can be deleted from a Container
co <- Container$new(1, 2, 3)
co$delete(3)
ee(co$values(), list(1, 2))

co <- Container$new(mean, identity)
ee(co$delete(mean)$values(), list(identity))
expect_error(co$delete(), 'argument "elem" is missing, with no default')

# Container gives an error if trying to delete non-existing element
co <- Container$new(1)
expect_error(co$delete(5), "5 is not in Container")
li = list(1, 2)
expect_error(co$delete(li), "list\\(1, 2\\) is not in Container")

# If duplicates, only one element is deleted
co <- Container$new(1, 2, 1)
co$delete(1)
ee(co, Container$new(1, 2))

# -------
# discard
# -------
# elements can be discarded from a Container
co <- Container$new(1, 2, 3)
co$discard(3)
ee(co$values(), list(1, 2))

co <- Container$new(mean, identity)
ee(co$discard(mean)$values(), list(identity))
expect_error(co$discard(), 'argument "elem" is missing, with no default')

# Container is not changed when trying to discard non-existing element
co <- Container$new(1)
expect_silent(co$discard(5))

# Multiple elements are all discarded
co <- Container$new(1, 2, 1)
co$discard(1)
ee(co, Container$new(1, 2))

# -----
# empty
# -----
# it can be checked whether the Container is empty
expect_true(Container$new()$is_empty())
expect_false(Container$new(numeric())$is_empty())
expect_false(Container$new(1)$is_empty())

# ---------------
# get_compare_fun
# ---------------

# ---
# has
# ---
co <- Container$new()
expect_false(co$has(NULL))
expect_true(co$add(NULL)$has(NULL))

foo <- function() print("foo")
co <- Container$new(mean, foo, identity)
expect_true(co$has(identity))
expect_true(co$has(mean))
expect_false(co$has(median))
expect_true(co$has(function() print("foo")))
expect_false(co$has(function() print("bar")))

co = Container$new(1, "1", integer(), NA)
expect_true(co$has(1))
expect_true(co$has("1"))
expect_true(co$has(integer()))
expect_true(co$has(NA))
expect_false(co$has(as.numeric(NA)))

# Due to all.equal being the default compare function the following also holds:
expect_true(co$has(numeric()))
expect_true(co$has(1L))

# This can be changed by using identical as the comparison function:
container_options(compare = identical)
co.ident = Container$new(co$values())
expect_false(co.ident$has(numeric()))
expect_false(co.ident$has(1L))
container_options(.reset = TRUE)

# ------
# length
# ------
# the length of a Container can be retrieved
ee(Container$new()$length(), 0)
co <- Container$new(1, 2, 3)
ee(co$length(), length(co$values()))

# -------
# peek_at
# -------
co = container(a = 1, 2, b = 3, 4)
ee(co$peek_at(1), container(a = 1))
ee(co$peek_at(2), container(2))
ee(co$peek_at(c("a", "b")), container(a = 1, b = 3))
ee(co$peek_at(list(1, "b")), container(a = 1, b = 3))
ee(co$peek_at(), co)
ee(co$peek_at(default = 1), co)

ee(co$peek_at(c(1, 1)), Container$new(a = 1, a = 1))
ee(co$peek_at(c("a", "a")), Container$new(a = 1, a = 1))

ee(co$peek_at(1:2), container(a = 1, 2))
ee(co$peek_at("a"), co$peek_at(match("a", names(co))))
ee(co$peek_at("b"), co$peek_at(match("b", names(co))))

ee(co$peek_at(0), container())
ee(co$peek_at(-1), container())
ee(co$peek_at("c"), container())
ee(co$peek_at(as.numeric(NA)), container())
ee(co$peek_at(0, default = "foo"), container("foo"))
ee(co$peek_at("z", default = "foo"), container(z = "foo"))

ee(co$peek_at(list("a", "x", 9), default = 0), container(a = 1, x = 0, 0))
ee(co$peek_at(c("a", "x", 9), default = 0), container(a = 1, x = 0, "9" = 0))
ee(co$peek_at(c(NA, NA), default = 0), container(0, 0))
ee(co$peek_at(NULL), container())
ee(co$peek_at(NULL, default = 0), container())
ee(co$peek_at(list(a = NULL), default = 0), container())
ee(co$peek_at(c(NULL, NA), default = 0), container(0))
ee(co$peek_at(c(NA, NULL), default = 0), container(0))

ee(co$peek_at(list("s1" = "a", "s2" = "x", "s3" = NULL), default = 0),
   container(a = 1, x = 0))

ee(co$peek_at("x", default = 1:3), container(x = 1:3))
ee(co$peek_at(list("s1" = "a", "s2" = "x", "s3" = 9), default = 1:3),
   container(a = 1, x = 1:3, s3 = 1:3))

# --------
# peek_at2
# --------
co = container(a = 1, 2, b = 3, 4)
ee(co$peek_at2(1), 1)
ee(co$peek_at2(2), 2)
ee(co$peek_at2("a"), 1)
ee(container()$peek_at2(1), NULL)
ee(container()$peek_at2(1, default = 0), 0)
ee(co$peek_at2(), NULL)
ee(co$peek_at2(default = 1), 1)

ee(co$peek_at2(1:2), NULL)
ee(co$peek_at2(1:2, default = 0), 0)
ee(co$peek_at2(0), NULL)
ee(co$peek_at2(0, default = "foo"), "foo")
ee(co$peek_at2(-1), NULL)
ee(co$peek_at2(-1, default = "foo"), "foo")
ee(co$peek_at2(5, default = 0), 0)
ee(co$peek_at2(as.numeric(NA), default = 0), 0)
ee(co$peek_at2(c("a", "b"), 0), 0)
ee(co$peek_at2("c", 0), 0)


# -----
# print
# -----
out = capture.output(print(Container$new()))
ee(out, "[]")

co = Container$new(1, 1L, NULL, integer())
out = capture.output(print(co))
ee(out, "[1, 1L, NULL, integer()]")

co2 = Container$new(list(), 3:5, co)
out = capture.output(print(co2))
ee(out, "[list(), (3L 4L 5L), [1, 1L, NULL, integer()]]")

# Increasing the size of the first container alters the output
co$add(1)$add(2)$add(3)
out = capture.output(print(co2))
ee(out, "[list(), (3L 4L 5L), <<Container(7)>>]")

co2$add(data.frame(A = 1:3, B = 3:1))
out = capture.output(print(co2))
ee(out, "[list(), (3L 4L 5L), <<Container(7)>>, <<data.frame(3x2)>>]")

# -------
# replace
# -------
# Requires two arguments old and new
expect_error(Container$new(0)$replace(0),
             'argument "new" is missing, with no default')
expect_error(Container$new(0)$replace(new = 1),
             'argument "old" is missing, with no default')

# By default signals an error if element does not exist
expect_error(Container$new()$replace(0, 1),
             "old element \\(0\\) is not in Container")
expect_error(Container$new()$replace(NULL, 1),
             "old element \\(NULL\\) is not in Container")
expect_error(Container$new(0)$replace(1, 2),
             "old element \\(1\\) is not in Container")

# If add == TRUE element is always added
ee(Container$new()$replace(0, 1, add = TRUE), Container$new(1))
ee(Container$new(1)$delete(1)$replace(0, 2, TRUE), Container$new(2))

# If multiple occurcenes, only one of them is replaced starting from the right
co = Container$new(1, 2, 1, 3)
co$replace(1, 0)
ee(co, Container$new(1, 2, 0, 3))

co = Container$new(1, 1L, "1")
co$replace(1, 0)
ee(co, Container$new(1, 0, "1"))

# Replace can replace special elements of basic type
co = Container$new(NULL, numeric(0), list(), NULL, numeric(0), list())
co$replace(NULL, 0)
ee(co, Container$new(NULL, numeric(), list(), 0, numeric(), list()))
co$replace(numeric(0), 0)
ee(co, Container$new(NULL, numeric(), list(), 0, 0, list()))
co$replace(list(), 0)
ee(co, Container$new(NULL, numeric(), list(), 0, 0, 0))

# Replace can replace by special elements of basic type
co = Container$new(0)
ee(co$replace(0, NULL), container(NULL))
ee(co$replace(NULL, numeric()), container(numeric()))
ee(co$replace(numeric(), list()), container(list()))


# Replace works on Container objects
co1 = Container$new(1)
co2 = Container$new(2)
co = Container$new(co1, co2, co1, co2)
co$replace(co1, 1)
ee(co, Container$new(co1, co2, 1, co2))
co$replace(co2, 2)
ee(co, Container$new(co1, co2, 1, 2))


# ------
# values
# ------
# the internal list of values of a Container can be retrieved
ee(Container$new()$values(), list())
ee(Container$new(1, 2, NULL)$values(), list(1, 2, NULL))


# -----
# clone
# -----
# Container objects provide reference semantics but can also be cloned
c1 <- Container$new(1, 2, 3)
c2 <- c1
cc <- c1$clone()
expect_true(identical(c1, c2))
expect_false(identical(c1, cc))
ee(c1$length(), cc$length())
ee(c1, cc)

c1$delete(3)
expect_true(identical(c1, c2))
expect_true(c1$length() < cc$length())

# Container objects can be even cloned deeply
c1 = Container$new(a = 1)
cc.deep = c1$clone(deep = TRUE)
ee(c1$values(), cc.deep$values())

c2 = Container$new(c1)

cc = c2$clone()
ee(cc, Container$new(Container$new(a = 1)))
c1$add(2)   # since it was not a deep clone, this will affect cc
ee(cc, Container$new(Container$new(a = 1, 2)))

cc.deep = c2$clone(deep = TRUE)
c1$add(3)   # this again affects cc but not cc.deep
ee(cc, Container$new(Container$new(a = 1, 2, 3)))
ee(cc.deep, Container$new(Container$new(a = 1, 2)))


# --------
# Iterator
# --------
# Iterator can be constructed from Container
v <- 1:5
co <- as.container(v)
it <- co$iter()
sum <- 0
while(it$has_next())
    sum <- sum + it$get_next()[[1]]

ee(sum(v), sum(as.integer(co$values())))


