# ----------
# initialize
# ----------
co <- Container$new()
expect_equal(attr(co, "class"), c("Container", "Iterable", "R6"))
expect_equal(mode(co$values()), "list")

co <- Container$new(environment())
expect_equal(co$length(), 1)

co <- Container$new(environment(), foo = identity)
expect_equal(co$length(), 2)

co <- Container$new(A = 1, B = 2)
expect_true(is.null(names(co$values())))
co <- Container$new(A = 1, B = 2, keep_names = TRUE)
expect_equal(names(co$values()), c("A", "B"))

expect_equal(Container$new(keep_names = TRUE),
             Container$new(keep_names = FALSE))


# ---
# add
# ---
co <- Container$new()
expect_true(co$empty())
co$add(1)
expect_equal(co$values(), list(1))


# NULL and empty lists can be added to and deleted from a Container
co <- Container$new()
co$add(NULL)
co$add(list())
co$add(0)
co$add(NULL)
co$add(list())

expect_equal(co$length(), 5)
co$delete(NULL)
co$delete(list())
expect_equal(co$length(), 3)
co$delete(list())
co$delete(NULL)
expect_equal(co$values(), list(0))

# non-trivial objects are added correctly
v <- 1:10
env <- new.env()
ll <- list(1, 2, "A")
foo <- function() print("foo")
collection <- c(list(v), list(env), list(ll), list(foo))

co <- Container$new()
co$add(v)$add(env)$add(ll)$add(foo)
expect_equal(co$values(), collection)
expect_equal(co$length(), length(collection))

# a Container can be added to a Container
co <- Container$new(1, 2)
coco <- Container$new()
coco$add(co)
expect_equal(coco$values()[[1]], co)
expect_equal(coco$values()[[1]]$values(), list(1, 2))

# named elements can be added to a Container
co <- Container$new()
x <- 1
names(x) <- "x"
co$add(x)
expect_equal(co$values(), list(c(x = 1)))

# ------
# delete
# ------
# elements can be deleted from a Container
co <- Container$new(1, 2, 3)
co$delete(3)
expect_equal(co$values(), list(1, 2))

co <- Container$new(mean, identity)
expect_equal(co$delete(mean)$values(), list(identity))
expect_error(co$delete(), 'argument "elem" is missing, with no default')

# Container gives an error if trying to delete non-existing element
co <- Container$new(1)
expect_error(co$delete(5), "5 is not in Container")
li = list(1, 2)
expect_error(co$delete(li), "li is not in Container")

# Elements are deleted according to LIFO principle
co <- Container$new(1, 2, 1)
co$delete(1)
expect_equal(co, Container$new(1, 2))

# -------
# discard
# -------
# elements can be discarded from a Container
co <- Container$new(1, 2, 3)
co$discard(3)
expect_equal(co$values(), list(1, 2))

co <- Container$new(mean, identity)
expect_equal(co$discard(mean)$values(), list(identity))
expect_error(co$discard(), 'argument "elem" is missing, with no default')

# Container is not changed when trying to discard non-existing element
co <- Container$new(1)
expect_silent(co$discard(5))

# Elements are discarded according to LIFO principle
co <- Container$new(1, 2, 1)
co$discard(1)
expect_equal(co, Container$new(1, 2))


# -----
# empty
# -----

# it can be checked whether the Container is empty
expect_true(Container$new()$empty())
expect_false(Container$new(numeric())$empty())
expect_false(Container$new(1)$empty())

# ---
# has
# ---

# it can be determined whether Container contains a certain element
co <- Container$new(1L)
expect_true(co$has(1L))
expect_false(co$has(7L))
expect_true(co$add(7L)$has(7L))

foo <- function() print("foo")
co <- Container$new(mean, foo, identity)
expect_true(co$has(identity))
expect_true(co$has(mean))
expect_false(co$has(median))
expect_true(co$has(function() print("foo")))
expect_false(co$has(function() print("bar")))


# the length of a Container can be retrieved
expect_equal(Container$new()$length(), 0)
co <- Container$new(1, 2, 3)
expect_equal(co$length(), length(co$values()))

# the data values of a Container can be retrieved
expect_equal(Container$new()$values(), list())
expect_equal(Container$new(1, 2, 3)$values(), list(1, 2, 3))

# Container objects provide reference semantics but can also be cloned
c1 <- Container$new(1, 2, 3)
c2 <- c1
cc <- c1$clone()
expect_true(identical(c1, c2))
expect_false(identical(c1, cc))
expect_equal(c1$length(), cc$length())
expect_equal(c1, cc)

c1$delete(3)
expect_true(identical(c1, c2))
expect_true(c1$length() < cc$length())


# Iterator can be constructed from Container
v <- 1:5
co <- as.container(v)
it <- co$iter()
sum <- 0
while(it$has_next()) sum <- sum + it$get_next()
expect_equal(sum(v), sum(as.integer(co$values())))

# verify that type() is deprecated
co <- Container$new()
expect_warning(co$type(), "deprecated")

