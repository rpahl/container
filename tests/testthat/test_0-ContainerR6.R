
describe(
    "initialize",
{
    ee <- expect_equal

    it("has the expected class attributes and mode",
    {
        co <- Container$new()
        ee(attr(co, "class"), c("Container", "Iterable", "R6"))
        ee(mode(co$values()), "list")
    })

    it("can be initialized with an environment",
    {
        co <- Container$new(environment())
        ee(co$length(), 1)

        co <- Container$new(environment(), foo = identity)
        ee(co$length(), 2)
    })

    it("keeps initialized names",
    {
        co <- Container$new(A = 1, B = 2)
        ee(names(co$values()), c("A", "B"))
    })
})


describe(
    "add",
{
    ee <- expect_equal

    test_that("adding simple number works as expected",
    {
        co <- Container$new()
        expect_true(co$is_empty())
        co$add(1)
        ee(co$values(), list(1))
    })

    test_that("NULL and empty lists can be added",
    {
        co <- Container$new()
        co$add(NULL)
        co$add(list())
        co$add(0)
        co$add(NULL)
        co$add(list())
        ee(co$values(), list(NULL, list(), 0, NULL, list()))
    })

    test_that("non-trivial objects are added correctly",
    {
        v <- 1:10
        env <- new.env()
        ll <- list(1, 2, "A")
        foo <- function() print("foo")
        collection <- c(list(v), list(env), list(ll), list(foo))

        co <- Container$new()
        co$add(v)$add(env)$add(ll)$add(foo)
        ee(co$values(), collection)
        ee(co$length(), length(collection))
    })

    test_that("a Container can be added to a Container",
    {
        co <- Container$new(1, 2)
        coco <- Container$new()
        coco$add(co)
        ee(coco$values()[[1]], co)
        ee(coco$values()[[1]]$values(), list(1, 2))
    })

    test_that("named elements can be added to a Container",
    {
        co <- Container$new()
        co$add(1, "a")$add(2)$add(3, "b")
        ee(co$values(), list(a = 1, 2, b = 3))
    })


    test_that("vectors are added correctly",
    {
        co <- Container$new()
        ee(co$add(1:5), Container$new(1:5))
        ee(co$add(NULL), Container$new(1:5, NULL))
        ee(co$add(list(1, 2)), Container$new(1:5, NULL, list(1, 2)))
    })
})


describe("at",
{
    ee <- expect_equal
    co <- Container$new(a = 1, 2, b = 3, 4)

    it("returns empty container if NULL was selected",
    {
        ee(co$at(NULL), Container$new())
    })

    it("selects by numeric index as expected",
    {
        ee(co$at(1), Container$new(a = 1))
        ee(co$at(2), Container$new(2))
        ee(co$at(1:2), Container$new(a = 1, 2))
        ee(co$at(c(1, 1)), Container$new(a = 1, a = 1))

        ee(co$at(c("a", "b")), Container$new(a = 1, b = 3))
        ee(co$at(list(1, "b")), Container$new(a = 1, b = 3))
    })

    it("selects by alphabetic index as expected",
    {
        ee(co$at("a"), co$at(match("a", names(co))))
        ee(co$at("b"), co$at(match("b", names(co))))
        ee(co$at(c("a", "a")), Container$new(a = 1, a = 1))
    })

    it("signals bad indices",
    {
        expect_error(co$at(), "'index' is missing")
        expect_error(co$at(0), "index must be > 0")
        expect_error(co$at("c"), "index 'c' not found")
        expect_error(co$at(as.numeric(NA)), "index must not be 'NA'")

        # TODO: allow negative indices
        expect_error(co$at(-1), "index must be > 0")
    })
})


describe("at2",
{
    ee <- expect_equal
    co <- Container$new(a = 1, 2, b = 3, 4)

    it("selects by numeric index as expected",
    {
        ee(co$at2(1), 1)
        ee(co$at2(2), 2)
    })

    it("selects by alphabetic index as expected",
    {
        ee(co$at2("a"), 1)
    })

    it("signals bad indices",
    {
        eer <- expect_error
        eer(co$at2(NULL), "index must be of length 1")
        eer(co$at2(), "'index' is missing")
        eer(co$at2(1:2), "index must be of length 1")
        eer(co$at2(0), "index must be > 0")
        eer(co$at2(5), "index 5 exceeds length of Container, which is 4")
        eer(co$at2(as.numeric(NA)), "index must not be 'NA'")
        eer(co$at2(c("a", "b")), "index must be of length 1")
        eer(co$at2("c"), "index 'c' not found")
        eer(co$at2(-1), "index must be > 0")
    })
})


describe("clear",
{
    ee <- expect_equal

    test_that("it clears as expected",
    {
        ee(Container$new()$clear(), Container$new())
        ee(Container$new(1, 2)$clear(), Container$new())
        ee(Container$new(a = 1, b = 2)$clear(), Container$new())
    })
})


describe("count",
{
    ee <- expect_equal

    test_that("number of element occurrences can be counted",
    {
        co <- Container$new("a", "a", "b", "a", "c")
        ee(co$count("a"), 3)
        ee(co$count("b"), 1)
        ee(co$count("c"), 1)
        ee(co$count("d"), 0)
    })
})


describe("delete",
{
    ee <- expect_equal
    co <- Container$new(1, 2, 3)

    test_that("elements can be deleted from a Container",
    {
        co$delete(3)
        ee(co$values(), list(1, 2))
    })

    test_that("it works with functions",
    {
        co <- Container$new(mean, identity)
        ee(co$delete(mean)$values(), list(identity))
    })

    test_that("signals missing element",
    {
        expect_error(
            co$delete(),
            'argument "elem" is missing, with no default'
        )
    })

    test_that("it gives an error if trying to delete non-existing element",
    {
        co <- Container$new(1)
        expect_error(co$delete(5), "5 is not in Container")
        li = list(1, 2)
        expect_error(co$delete(li), "list\\(1, 2\\) is not in Container")
    })

    test_that("If duplicates, only one element is deleted",
    {
        co <- Container$new(1, 2, 1)
        co$delete(1)
        ee(co, Container$new(2, 1))
    })
})


describe("delete_at",
{
    ee <- expect_equal
    co <- Container$new(a = 1, 2, 3)

    test_that("elements can be deleted at specific indexes",
    {
        ee(co$delete_at(3), Container$new(a = 1, 2))
        ee(co$delete_at("a")$values(), list(2))

        co <- Container$new(mean, identity)
        ee(co$delete_at(1), Container$new(identity))
    })

    test_that("signals missing index",
    {
        expect_error(co$delete_at(), "'index' is missing")
    })

    test_that("it gives an error if indices dont not exist",
    {
        co <- Container$new(1)
        expect_error(
            co$delete_at(5),
            "index 5 exceeds length of Container, which is 1"
        )
        expect_error(
            co$delete_at("a"),
            "index 'a' not found"
        )
    })
})


describe("discard",
{
    ee <- expect_equal

    test_that("elements can be discarded from a Container",
    {
        co <- Container$new(1, 2, 3)
        ee(co$discard(3)$values(), list(1, 2))

        co <- Container$new(mean, identity)
        ee(co$discard(mean)$values(), list(identity))
        expect_error(
            co$discard(),
            "argument \"elem\" is missing, with no default"
        )
    })

    test_that(
        "container is not changed when trying to discard non-existing element",
    {
        co <- Container$new(1)
        expect_silent(ee(co$discard(5), co))
        co <- Container$new()
        expect_silent(ee(co$discard(0), co))
    })

    test_that("it works on empty containers",
    {
        ee(Container$new()$discard_at(1), Container$new())
    })
})


describe(
    "discard_at",
{
    ee <- expect_equal

    test_that(
        "elements can be discarded at certain index",
    {
        co <- Container$new(a = 1, 2, 3)
        ee(co$discard_at(3), Container$new(a = 1, 2))
        ee(co$discard_at("a"), Container$new(2))

        co <- Container$new(mean, identity)
        ee(co$discard_at(1), Container$new(identity))
    })

    test_that(
        "container is not changed when trying to discard at non-existing index",
    {
        co <- Container$new(1)
        expect_silent(ee(co$discard_at(5), co))
        co <- Container$new()
        expect_silent(ee(co$discard_at(0), co))
    })

    test_that("it works on empty containers",
    {
        ee(Container$new()$discard_at(1), Container$new())
    })

    it("signals missing index",
    {
        co <- Container$new(1)
        expect_error(co$discard_at(), "'index' is missing")
    })
})


describe("get_compare_fun",
{
    test_that(
        "the internally used comparison function can be returned",
    {
        co <- Container$new()
        res <- co$get_compare_fun()
        expected <- get(container_options("compare")[[1]])
        expect_equal(res, expected)

        on.exit(container_options(.reset = TRUE))
        container_options("compare" = identical)
        co <- Container$new()
        res <- co$get_compare_fun()
        expect_equal(res, identical)
    })
})


describe("has",
{
    test_that("it can be checked if a container as some element",
    {
        co <- Container$new(1, "1", integer(), NA)
        expect_true(co$has(1))
        expect_true(co$has("1"))

        # Due to all.equal being the default internal comparison function,
        # the following also holds:
        expect_true(co$has(numeric()))
        expect_true(co$has(1L))

        # This can be changed by using identical as the comparison function:
        container_options(compare = identical)
        on.exit(container_options(.reset = TRUE))
        co.ident <- as.container(co$values())
        expect_false(co.ident$has(numeric()))
        expect_false(co.ident$has(1L))
    })

    test_that("it works as expected with special and zero-length elements",
    {
        co <- Container$new(integer(), NA_real_)
        expect_true(co$has(integer()))
        expect_true(co$has(NA_real_))
        expect_false(co$has(NA_character_))
        expect_false(co$has(NULL))
        expect_true(co$add(NULL)$has(NULL))
    })

    test_that("it works with functions",
    {
        foo <- function() print("foo")
        co <- Container$new(mean, foo, identity)
        expect_true(co$has(identity))
        expect_true(co$has(mean))
        expect_false(co$has(median))
        expect_true(co$has(function() print("foo")))
        expect_false(co$has(function() print("bar")))
    })
})


describe("has_name",
{
    it("can be checked if a container has an element with a certain name",
    {
        expect_false(Container$new()$has_name())
        expect_false(Container$new(1)$has_name())
        expect_false(Container$new("a")$has_name())
        expect_true(Container$new(a = 1)$has_name())

        co <- Container$new(a = 1, 2, b = 3, 4)
        expect_true(co$has_name("a"))
        expect_true(co$has_name("b"))
        expect_false(co$has_name("2"))
    })

    it("signals bad name arguments",
    {
        co <- Container$new(a = 1, b = 2)

        expect_error(
            co$has_name(NULL),
            "name must be a character string, but got 'NULL'"
        )
        expect_error(
            co$has_name(c("a", "b")),
            "name must be of length 1"
        )
        expect_error(
            co$has_name(as.character(NA)),
            "undefined name"
        )
        expect_error(
            co$has_name(""),
            "name string must not be empty"
        )
    })
})


describe("is_empty",
{
    it("it can be checked whether the Container is empty",
    {
        expect_true(Container$new()$is_empty())
        expect_false(Container$new(numeric())$is_empty())
        expect_false(Container$new(1)$is_empty())
    })
})


describe("length",
{
    test_that("the length of a Container can be retrieved",
    {
        expect_equal(Container$new()$length(), 0)

        co <- Container$new(1, 2, 3)
        expect_equal(co$length(), length(co$values()))
    })
})


describe("names",
{
    test_that("all names can be listed",
    {
        co <- Container$new(a = 1, b = 2, y = 3)
        expect_equal(co$names(), c("a", "b", "y"))
    })

    test_that("the name listing is consistent with R lists",
    {
        l <- list(a = 1, 2, y = 3)
        co <- as.container(l)
        expect_equal(co$names(), names(l))

        l <- list(1, 2, 3)
        co <- as.container(l)
        expect_equal(co$names(), names(l))
    })
})


describe("peek_at",
{
    ee <- expect_equal
    co <- container(a = 1, 2, b = 3, 4)
    l <- as.list(co)

    test_that("elements can be peeked at by index",
    {
        ee(co$peek_at(1), container(a = 1))
        ee(co$peek_at(2), container(2))
    })

    test_that("elements can be peeked at by name",
    {
        ee(co$peek_at("a"), container(a = 1))
        ee(co$peek_at("b"), container(b = 3))
    })

    test_that("it works with vector input",
    {
        ee(co$peek_at(c("a", "b")), container(a = 1, b = 3))
        ee(co$peek_at(1:2), container(a = 1, 2))
    })

    test_that("it works with mixed list input",
    {
        ee(co$peek_at(list(1, "b")), container(a = 1, b = 3))
    })

    test_that("if no arg is given, as with R lists [ operator,
        the entire container is returned as is",
    {
        ee(l[], l)
        ee(co$peek_at(), co)
        ee(co$peek_at(default = 1), co)
    })

    test_that("it accepts duplicated indices",
    {
        ii <- c(1, 1)
        ee(l[ii], list(a = 1, a = 1))
        ee(co$peek_at(c(1, 1)), Container$new(a = 1, a = 1))
        ee(co$peek_at(c("a", "a")), Container$new(a = 1, a = 1))
    })

    test_that(
        "single invalid indices are ignored and return empty container",
    {
        co <- container(a = 1, 2, b = 3, 4)

        ee(co$peek_at(0), container())
        ee(co$peek_at(-1), container())
        ee(co$peek_at("c"), container())
        ee(co$peek_at(NA_real_), container())
        ee(co$peek_at(NULL), container())
    })

    test_that(
        "if default is given, it is returned in case of invalid index",
    {
        co <- container(a = 1, 2, b = 3, 4)
        ee(co$peek_at(0, default = "foo"), container("foo"))
        ee(co$peek_at("z", default = "foo"), container(z = "foo"))
    })

    test_that(
        "default works with multi-length input",
    {
        co <- container(a = 1, 2, b = 3, 4)

        ee(
            co$peek_at(list("a", "x", 9), default = 0),
            container(a = 1, x = 0, 0)
        )
        ee(
            co$peek_at(c("a", "x", 9), default = 0),
            container(a = 1, x = 0, "9" = 0)
        )
        ee(
            co$peek_at(list("s1" = "a", "s2" = "x", "s3" = NULL), default = 0),
            container(a = 1, x = 0)
        )
        ee(co$peek_at(c(NA, NA), default = 0), container(0, 0))
        ee(co$peek_at(NULL, default = 0), container())
        ee(co$peek_at(list(a = NULL), default = 0), container())
        ee(co$peek_at(c(NULL, NA), default = 0), container(0))
        ee(co$peek_at(c(NA, NULL), default = 0), container(0))
    })

    test_that(
        "default can be a vector as well",
    {
        co <- container(a = 1, 2, b = 3, 4)

        ee(co$peek_at("x", default = 1:3), container(x = 1:3))
        ee(
            co$peek_at(list("s1" = "a", "s2" = "x", "s3" = 9), default = 1:3),
            container(a = 1, x = 1:3, s3 = 1:3)
        )
    })
})


describe("peek_at2",
{
    ee <- expect_equal
    co <- container(a = 1, 2, b = 3, 4)
    l <- as.list(co)

    test_that("single element can be peeked at by index",
    {
        ee(co$peek_at2(1), 1)
        ee(co$peek_at2(2), 2)
        ee(co$peek_at2("a"), 1)
    })

    test_that(
        "invalid index returns NULL, unless different default is given",
    {

        ee(co$peek_at2(0), NULL)
        ee(co$peek_at2(0, default = "foo"), "foo")
        ee(co$peek_at2(-1), NULL)
        ee(co$peek_at2(-1, default = "foo"), "foo")
        ee(co$peek_at2(99), NULL)
        ee(co$peek_at2(99, default = 0), 0)
        ee(co$peek_at2("c", 0), 0)
    })

    test_that("if no arg is given, NULL is returned, unless
        different default is given",
    {
        ee(co$peek_at2(), NULL)
        ee(co$peek_at2(default = 1), 1)
    })

    test_that("bad indices are signaled",
    {
        expect_error(co$peek_at2(1:2), "index must be of length 1")
        expect_error(co$peek_at2(c("a", "b")), "index must be of length 1")
        expect_error(co$peek_at2(NA_real_), "index must not be 'NA'")
    })
})


describe("pop",
{
    ee <- expect_equal

    test_that(
        "elements can be popped at given index or name",
    {
        co <- container(a = 1, 2, b = 3, 4)

        ee(co$pop("b"), 3)
        ee(co, container(a = 1, 2, 4))

        ee(co$pop(1), 1)
        ee(co, container(2, 4))
    })

    test_that("it signals bad index",
    {
        co <- container(a = 1, b = 2)
        expect_error(co$pop(0), "index must be > 0")
        expect_error(co$pop(-1), "index must be > 0")

        expect_error(
            co$pop(3),
            "index 3 exceeds length of Container, which is 2"
        )
    })

    test_that("it signals when trying to pop from empty container",
    {
        expect_error(container()$pop(), "pop at empty Container")
    })

    test_that(
        "if no arg is given, the last element is popped",
    {
        co <- container(a = 1, 2, b = 3, 4)

        ee(co$pop(), 4)
        ee(co, container(a = 1, 2, b = 3))

        ee(co$pop(), 3)
        ee(co, container(a = 1, 2))
    })
})


describe("print",
{
    ee <- expect_equal

    test_that("it prints as expected",
    {
        out <- capture.output(print(Container$new()))
        ee(out, "[]")

        co <- Container$new(1, 1L, NULL, integer())
        out <- capture.output(print(co))
        ee(out, "[1, 1L, NULL, integer()]")

        co2 <- Container$new(list(), 3:5, co)
        out <- capture.output(print(co2))
        ee(out, "[list(), (3L 4L 5L), [1, 1L, NULL, integer()]]")

        # Increasing the size of the first container alters the output
        co$add(1)$add(2)$add(3)
        out <- capture.output(print(co2))
        ee(out, "[list(), (3L 4L 5L), <<Container(7)>>]")

        co2$add(data.frame(A = 1:3, B = 3:1))
        out <- capture.output(print(co2))
        ee(out, "[list(), (3L 4L 5L), <<Container(7)>>, <<data.frame(3x2)>>]")
    })
})



describe("rename",
{
    ee <- expect_equal

    test_that("values can be renamed",
    {
        co <- Container$new(A = 1, B = 2, 3, D = 4)
        vals <- as.numeric(co$values())

        co$rename("A", "a")
        expect_equal(names(co), c("a", "B", "", "D"))

        # Verify that values did not change
        values_did_not_change <- all.equal(vals, as.numeric(co$values()))
        expect_true(values_did_not_change)
    })

    test_that("several values can be renamed at once",
    {
        co <- Container$new(A = 1, B = 2, 3, D = 4)

        co$rename(c("A", "B"), c("x", "y"))
        ee(names(co), c("x", "y", "", "D"))

        co$rename("D", "4")
        ee(names(co), c("x", "y", "", "4"))
    })

    test_that("signals badly typed arguments",
    {
        co <- Container$new(A = 1, B = 2, 3, D = 4)

        expect_error(co$rename(1, "C"), "'old' must be character")
        expect_error(co$rename("A", 1), "'new' must be character")
    })

    test_that("signals if arguments do not match in length",
    {
        co <- Container$new(A = 1, B = 2)

        expect_error(
            co$rename("A", c("C", "D")),
            "'old' and 'new' names must be of the same length"
        )
    })

    test_that("signals if name already exists",
    {
        expect_error(
            Container$new(A = 1, B = 2)$rename("A", "B"),
            "name 'B' already in Container"
        )
    })

    test_that("signals if name already exists",
    {
        expect_error(
            Container$new(A = 1, B = 2)$rename("Z", "B"),
            "Items of 'old' not found in names: 'Z'"
        )
    })

    test_that("signals if name does not exist",
    {
        expect_error(
            Container$new(x = 1, y = 2)$rename(c("x", "x2"), c("x2", "x3")),
            "Items of 'old' not found in names: 'x2'"
        )
    })
})


describe("replace",
{
    ee <- expect_equal

    test_that("it can replace simple elements",
    {
        ee(
            Container$new(1, 2)$replace(1, 9),
            Container$new(9, 2)
        )
        ee(
            Container$new(1, 2)$replace(2, 9),
            Container$new(1, 9)
        )
    })

    test_that("works with Container objects",
    {
        co1 <- Container$new(1)
        co2 <- Container$new(2)
        co <- Container$new(co1, co2, co1, co2)

        ee(co$replace(co1, 1), Container$new(1, co2, co1, co2))
        ee(co$replace(co2, 2), Container$new(1, 2, co1, co2))
    })


    test_that("it signals missing elements",
    {
        co <- Container$new(1, 2)
        expect_error(
            co$replace(old = 0, new = 1),
            "old element \\(0\\) is not in Container"
        )
        expect_error(
            co$replace(old = NULL, new = 1),
            "old element \\(NULL\\) is not in Container"
        )
    })

    test_that("it can add missing elements",
    {
        ee(Container$new()$replace(0, 1, add = TRUE), Container$new(1))
        ee(Container$new(1)$delete(1)$replace(0, 2, TRUE), Container$new(2))
    })

    test_that("if multiple occurcenes, only one of them is
        replaced starting from the left",
    {
        co <- Container$new(1, 2, 1, 3)
        ee(co$replace_at(1, 0), Container$new(0, 2, 1, 3))

        co <- Container$new("1", 1, 1)
        ee(co$replace(1, 0), Container$new("1", 0, 1))
    })

    test_that("can replace special elements of basic type",
    {
        co <- Container$new(NULL, numeric(0), list(), NULL, numeric(0), list())

        ee(co$replace(NULL, 0),
        Container$new(0, numeric(), list(), NULL, numeric(), list()))

        ee(co$replace(numeric(0), 0),
        Container$new(0, 0, list(), NULL, numeric(), list()))

        ee(co$replace(list(), 0),
        Container$new(0, 0, 0, NULL, numeric(), list()))
    })

    test_that("can replace *by* special elements of basic type",
    {
        co <- Container$new(0)
        ee(co$replace(0, NULL), container(NULL))
        ee(co$replace(NULL, numeric()), container(numeric()))
        ee(co$replace(numeric(), list()), container(list()))
    })

    test_that("replacing a named element preserves the name",
    {
        co <- Container$new(a = 1, b = 2)
        ee(co$replace(1, 0), Container$new(a = 0, b = 2))
    })
})


describe("replace_at",
{
    ee <- expect_equal

    test_that("it can replace at indices",
    {
        ee(
            Container$new(a = 1, b = 2)$replace_at(1, 9),
            Container$new(a = 9, b = 2)
        )
        ee(
            Container$new(a = 1, b = 2)$replace_at("a", 9),
            Container$new(a = 9, b = 2)
        )
        ee(
            Container$new(a = 1, b = 2)$replace_at(2, 9),
            Container$new(a = 1, b = 9)
        )
        ee(
            Container$new(a = 1, b = 2)$replace_at("b", 9),
            Container$new(a = 1, b = 9)
        )
    })

    test_that("signals missing arguments",
    {
        co <- Container$new(a = 1, b = 2)

        # Requires two arguments index and value
        expect_error(
            co$replace_at(1),
            "argument \"value\" is missing, with no default"
        )
        expect_error(
            co$replace_at(value = 1),
            "'index' is missing"
        )
    })

    test_that("signals invalid indices",
    {
        co <- Container$new(a = 1, b = 2, c = 3)

        expect_error(
            co$replace_at(0, 9),
            "index must be > 0"
        )
        expect_error(
            co$replace_at(4, 9),
            "index 4 exceeds length of Container, which is 3"
        )
        expect_error(
            co$replace_at("x", 9),
            "index 'x' not found"
        )
    })

    test_that("If add == TRUE element is always added",
    {
        co <- Container$new()
        ee(co$replace_at(1, 9, add = TRUE), Container$new(9))
        ee(co$replace_at("b", 7, add = TRUE), Container$new(9, b = 7))
        ee(co$replace_at(10, NULL, add = TRUE), Container$new(9, b = 7, NULL))
    })

    test_that("Replace can replace by special elements of basic type",
    {
        co <- Container$new(1, b = 2, 3)
        ee(co$replace_at("b", NULL), Container$new(1, b = NULL, 3))
        ee(co$replace_at(3, numeric(0)), Container$new(1, b = NULL, numeric(0)))
    })

    test_that("it can replace by vectors",
    {
        co <- Container$new(1, b = 2, 3)
        ee(
            co$replace_at(1, 1:5),
            Container$new(1:5, b = 2, 3)
        )
        ee(
            co$replace_at("b", list(2, 3, 4)),
            Container$new(1:5, b = list(2, 3, 4), 3)
        )
    })
})


describe("update",
{
    ee <- expect_equal

    test_that("a Container can be updated by another Container object",
    {
        co <- Container$new()

        c1 <- Container$new(A = 0)
        co$update(c1)
        ee(co$values(), list(A = 0))

        c2 <- Container$new(A = 1, B = 2, C = 12)
        co$update(c2)
        ee(co$values(), list(A = 1, B = 2, C = 12))

        c3 <- Container$new(C = 3, D = 4)
        co$update(c3)
        ee(co$values(), list(A = 1, B = 2, C = 3, D = 4))
    })

    test_that(
        "it can be updated by another Container with unnamed elements",
    {
        co <- Container$new(a = 0)
        co$update(Container$new(2, a = 1, 1))
        ee(co$values(), list(a = 1, 2, 1))
    })

    test_that("if updated by itself, nothing changes",
    {
        co <- Container$new(A = 0)
        ee(co$update(co), co)
    })

    test_that("if updated by empty Container, nothing changes",
    {
        co <- Container$new(A = 0)
        ee(co$update(co), Container$new(A = 0))
    })

    test_that(
        "if empty Container is updated by another
        Container, it becomes the second container",
    {
        co <- Container$new(A = 0)
        ee(Container$new()$update(co), Container$new(A = 0))
    })

    test_that("if updated by list, an error is signaled",
    {
        co <- Container$new(a = 0)
        expect_error(co$update(list(a = 1)), "arg must be iterable")
    })
})


describe("values",
{
    ee <- expect_equal

    test_that(
        "the internal list of values of a Container can be retrieved",
    {
        ee(Container$new()$values(), list())
        ee(Container$new(1, 2, NULL)$values(), list(1, 2, NULL))

        co <- Container$new(a = 1, 2, 3)
        ee(co$values(), list(a = 1, 2, 3))
    })

    test_that("ensure that names become NULL if no name exists",
    {
        co <- Container$new(a = 1, 2, 3)$delete_at("a")
        ee(co$values(), list(2, 3))
        expect_true(is.null(names(co$values())))
    })
})


describe("clone",
{
    ee <- expect_equal

    test_that("containers can be cloned",
    {
        c1 <- Container$new(1, 2, 3)
        c2 <- c1
        cc <- c1$clone()

        expect_true(identical(c1, c2))
        expect_false(identical(c1, cc))

        ee(c1$length(), cc$length())
        ee(c1, cc)

        c1$delete(3)
        expect_true(identical(c1, c2))
        ee(cc$values(), list(1, 2, 3))
    })

    test_that("containers can be cloned deeply",
    {
        c1 <- Container$new(a = 1)
        cc.deep <- c1$clone(deep = TRUE)
        ee(c1$values(), cc.deep$values())

        c2 <- Container$new(c1)

        cc <- c2$clone()
        ee(cc, Container$new(Container$new(a = 1)))

        c1$add(2)   # since it was not a deep clone, this will affect cc
        ee(cc, Container$new(Container$new(a = 1, 2)))

        cc.deep <- c2$clone(deep = TRUE)
        c1$add(3)   # this again affects cc but not cc.deep
        ee(cc, Container$new(Container$new(a = 1, 2, 3)))
        ee(cc.deep, Container$new(Container$new(a = 1, 2)))
    })
})


describe("iter",
{
    test_that("iterator can be constructed from Container",
    {
        v <- 1:5
        co <- as.container(v)
        it <- co$iter()
        sum <- 0
        while(it$has_next()) {
            sum <- sum + it$get_next()[[1]]
        }

        expect_equal(sum(v), sum(as.integer(co$values())))
    })
})
