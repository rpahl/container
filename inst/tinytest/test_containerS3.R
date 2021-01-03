# Container initialization works as expected
co <- container()
expect_true(is.container(co))
expect_equal(attr(co, "class"), c("Container", "Iterable", "R6"))
expect_equal(mode(values(co)), "list")

co <- container(environment())
expect_equal(length(co), 1)

co <- container(environment(), foo = identity)
expect_equal(length(co), 2)

co <- container(A = 1, B = 2)
expect_true(is.null(names(values(co))))
co <- container(A = 1, B = 2, keep_names = TRUE)
expect_equal(names(values(co)), c("A", "B"))

expect_equal(container(keep_names = TRUE), container(keep_names = FALSE))


# it can be checked whether the Container is empty
expect_true(empty(container()))
expect_false(empty(container(numeric())))
expect_true(empty(as.container(numeric())))
expect_false(empty(container(1)))


# elements can be added to the Container
co <- container()
expect_true(empty(co))
add(co, 1)
expect_equal(values(co), list(1))


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


# non-trivial objects are added correctly
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
co <- as.container(v)
coco <- container()
add(coco, co)
expect_equal(values(coco)[[1]], co)
expect_equal(values(values(coco)[[1]]), as.list(v))


# named elements can be added to a Container
co <- container()
x <- 1
names(x) <- "x"
add(co, x)
expect_equal(values(co), list(c(x = 1)))


# it can be determined whether Container contains a certain element
co <- as.container(1:5)
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
x <- as.list(1:5)
co <- as.container(x)
discard(co, 3L)
expect_equal(values(co), x[-3])

co <- container(mean, identity)
expect_equal(values(discard(co, mean)), list(identity))

expect_error(discard(co), 'argument "elem" is missing, with no default')


# elements can be discarded from left and from right
co <- as.container(c(1, 2, 1))
expect_equal(values(discard(co, 1)), as.list(2:1))

co <- as.container(c(1, 2, 1))
expect_equal(values(discard(co, 1, right = TRUE)), as.list(1:2))


# discarding non-existent elements does not change Container
co <- as.container(1:3)
expect_equal(values(discard(co, 5)), as.list(1:3))
expect_equal(discard(container()), container())


# elements can be deleted from a Container
x <- as.list(1:5)
co <- as.container(x)
delete(co, 3L)
expect_equal(values(co), x[-3])

co <- container(mean, identity)
expect_equal(values(delete(co, mean)), list(identity))
expect_error(delete(co), 'argument "elem" is missing, with no default')


# Container gives an error if trying to delete non-existing element
co <- as.container(1:3)
expect_error(delete(co, 5L), "5 not in Container")


# elements can be deleted from left and from right
co <- as.container(c(1, 2, 1))
expect_equal(values(delete(co, 1)), as.list(2:1))

co <- as.container(c(1, 2, 1))
expect_equal(values(delete(co, 1, right = TRUE)), as.list(1:2))


# the length of a Container can be retrieved
expect_equal(length(container()), 0)
x <- 1:5
co <- as.container(x)
expect_equal(length(co), length(x))

ll <- list(mean, identity)
co <- as.container(ll)
expect_equal(length(co), length(ll))


# Container objects provide reference semantics but can also be cloned
c1 <- as.container(1:10)
c2 <- c1
cc <- clone(c1)
expect_true(identical(c1, c2))
expect_false(identical(c1, cc))
expect_equal(length(c1), length(cc))
expect_equal(c1, cc)

delete(c1, 7L)
expect_true(identical(c1, c2))
expect_true(length(c1) < length(cc))

# Container can be converted to list
expect_equal(as.list(container()), list())
expect_equal(as.list(as.container(1:10)), as.list(1:10))
expect_equal(as.list(container(NULL)), list(NULL))

# conversion to Container works as expected
expect_error(as.container(), '"x" is missing')
expect_equal(as.container(NULL), container())
expect_equal(as.container(1), container(1))
expect_equal(as.container(1:3), container(1, 2, 3))

expect_error(as.container(factor(letters[1:3])))
expect_error(as.container(globalenv()))

# container is printed as expected
out <- capture.output(print(Container$new()))
out.expected <- c("<Container> of 0 elements")
expect_equal(out, out.expected)

out <- capture.output(print(Container$new(1)))
out.expected <- c("<Container> of 1 element:",
                  " : num 1")
expect_equal(out, out.expected)

out <- capture.output(print(Container$new(1, 2)))
out.expected <- c("<Container> of 2 elements:",
                  " : num 1",
                  " : num 2")
expect_equal(out, out.expected)

