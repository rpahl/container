# Container initialization works as expected
co <- container()
expect_true(is.container(co))
expect_equal(attr(co, "class"), c("Container", "Iterable", "R6"))

co <- container(1:4)
expect_equal(mode(values(co)), "numeric")

co <- container(environment())
expect_equal(mode(values(co)), "list")
expect_equal(length(co), 1)

co <- container(environment(), foo = identity)
expect_equal(length(co), 2)

co <- container(A = 1, B = 2)
expect_true(is.null(names(values(co))))
co <- container(A = 1, B = 2, keep_names = TRUE)
expect_equal(names(values(co)), c("A", "B"))

expect_equal(container(keep_names = TRUE), container(keep_names = FALSE))


# type of Container is inialized as expected
expect_equal(mode(values(container())), "list")
expect_equal(mode(values(container(1))), "numeric")
expect_equal(mode(values(container(new.env()))), "list")
expect_equal(mode(values(container(TRUE))), "logical")
expect_equal(mode(values(container(function(){}))), "list")
expect_equal(mode(values(container(raw()))), "raw")
expect_equal(mode(values(container(0+0i))), "complex")
expect_equal(mode(values(container(letters[1:10]))), "character")


# it can be checked whether the Container is empty
expect_true(empty(container()))
expect_true(empty(container(numeric())))
expect_false(empty(container(1)))


# elements can be added to the Container
co <- container()
expect_true(empty(co))
add(co, 1)
expect_equal(values(co), list(1))

co <- container(numeric())
add(co, 1)
expect_equal(values(co), 1)


# NULL and empty lists can be added to a Container
co <- container()
add(co, NULL)
add(co, list())
add(co, 0)
add(co, NULL)
add(co, list())

expect_equal(length(co), 5)
delete(co, NULL)
delete(co, list())
expect_equal(length(co), 3)
delete(co, list())
delete(co, NULL)
expect_equal(values(co), list(0))

expect_true(empty(add(container(numeric(0)), numeric(0))))


# types of added elements must match for non-list Containers
co <- container(1)
expect_equal(values(co), 1)
add(co, 2)
expect_equal(values(co), 1:2)
expect_error(add(co, "a"), "type mismatch: expected 'numeric' but got 'character'")
expect_error(add(co, list(1)), "type mismatch: expected 'numeric' but got 'list'")
expect_error(add(co, list()), "type mismatch: expected 'numeric' but got 'list'")
expect_error(add(co, NULL), "type mismatch: expected 'numeric' but got 'NULL'")
expect_equal(values(add(co, 3:5)), 1:5)


# test_that("non-trivial objects are added correctly
v <- 1:10
env <- new.env()
ll <- list(1, 2, "A")
foo <- function() print("foo")
collection <- c(list(v), list(env), list(ll), list(foo))

co <- container()
for (elem in collection) add(co, elem)
expect_equal(values(co), collection)
expect_equal(length(co), length(collection))


# a Container can be added to a Container
v <- 1:10
co <- container(v)
coco <- container()
add(coco, co)
expect_equal(values(coco)[[1]], co)
expect_equal(values(values(coco)[[1]]), v)


# named elements can be added to a Container
co <- container(numeric())
x <- 1
names(x) <- "x"
add(co, x)

y <- 1:3
names(y) <- letters[1:3]
add(co, y)

expect_equal(values(co), c(x, y))


# a cleared Container preserves its type
expect_equal(mode(values(clear(container()))), "list")
expect_equal(mode(values(clear(container(1:3)))), "numeric")
expect_equal(mode(values(clear(container("a")))), "character")


# it can be determined whether Container contains a certain element
co <- container(1:5)
expect_true(has(co, 1L))
expect_false(has(co, 7))
expect_true(has(add(co, 7), 7))

foo <- function() print("foo")
co <- container(mean, foo, identity)
expect_true(has(co, identity))
expect_true(has(co, mean))
expect_false(has(co, median))
expect_true(has(co, function() print("foo")))
expect_false(has(co, function() print("bar")))


# elements can be discarded from a Container
x <- 1:5
co <- container(x)
discard(co, 3L)
expect_equal(values(co), x[-3])

co <- container(mean, identity)
expect_equal(values(discard(co, mean)), list(identity))

expect_error(discard(co), 'argument "elem" is missing, with no default')


# elements can be discarded from left and from right
co <- container(c(1, 2, 1))
expect_equal(values(discard(co, 1)), 2:1)

co <- container(c(1, 2, 1))
expect_equal(values(discard(co, 1, right = TRUE)), 1:2)


# discarding non-existent elements does not change Container
co <- container(1:3)
expect_equal(values(discard(co, 5)), 1:3)
expect_equal(discard(container()), container())


# elements can be deleted from a Container
x <- 1:5
co <- container(x)
delete(co, 3L)
expect_equal(values(co), x[-3])

co <- container(mean, identity)
expect_equal(values(delete(co, mean)), list(identity))
expect_error(delete(co), 'argument "elem" is missing, with no default')


# Container gives an error if trying to delete non-existing element
co <- container(1:3)
expect_error(delete(co, 5L), "5 not in Container")


# elements can be deleted from left and from right
co <- container(c(1, 2, 1))
expect_equal(values(delete(co, 1)), 2:1)

co <- container(c(1, 2, 1))
expect_equal(values(delete(co, 1, right = TRUE)), 1:2)


# the length of a Container can be retrieved
expect_equal(container()$length(), 0)
x <- 1:5
co <- container(x)
expect_equal(length(co), length(x))

ll <- list(mean, identity)
co <- container(ll)
expect_equal(length(co), length(ll))


# the data values of a Container can be retrieved
expect_equal(values(container()), list())
expect_equal(values(container(1:5)), 1:5)


# Container objects provide reference semantics but can also be cloned
c1 <- container(1:10)
c2 <- c1
cc <- clone(c1)
expect_true(identical(c1, c2))
expect_false(identical(c1, cc))
expect_equal(length(c1), length(cc))
expect_equal(c1, cc)

delete(c1, 7L)
expect_true(identical(c1, c2))
expect_lt(length(c1), length(cc))


# Iterator can be constructed from Container
v <- 1:5
co <- container(v)
it <- iter(co)
sum <- 0
while(has_next(it)) sum <- sum + get_next(it)
expect_equal(sum(v), sum(values(co)))


# Container object can be converted to base list
expect_equal(as.list(container(1:5)), as.list(1:5))
expect_equal(as.list(container("A", mean, globalenv())),
             list("A", mean, globalenv()))

