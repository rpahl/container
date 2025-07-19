
describe("initialize",
{
    d <- Dict$new()

    test_that("has the expected class attributes",
    {
        expect_equal(
            attr(d, "class"),
            c("Dict", "Container", "Iterable", "R6")
        )
    })

    test_that("values are stored as a list",
    {
        expect_equal(mode(d$values()), "list")
    })

    test_that("works with environments",
    {
        d <- Dict$new(env = environment())
        expect_equal(d$length(), 1)

        d <- Dict$new(env = environment(), foo = identity)
        expect_equal(d$length(), 2)
    })

    test_that("signals unnamed elements",
    {
        expect_error(Dict$new(1:2), "all elements must be named")
    })

    test_that("signals duplicated keys",
    {
        expect_error(Dict$new(x = 1, y = 2, x = 3), "duplicated keys")
    })

    test_that("initializing with no element",
    {
        expect_equal(Dict$new()$values(), list())
        expect_equal(Dict$new()$keys(), character(0))
    })

    test_that("initializing with one element",
    {
        expect_equal(Dict$new(x = 1)$values(), list(x = 1))
        expect_equal(Dict$new(x = NULL)$values(), list(x = NULL))
        expect_equal(Dict$new(x = 1:4)$values(), list(x = 1:4))
        env <- new.env()
        expect_equal(Dict$new(x = env)$values(), list(x = env))
    })

    test_that("initializing with two or more elements",
    {
        d <- Dict$new(x = 1:2, y = 2:3)
        expect_equal(d$values(), Container$new(x = 1:2, y = 2:3)$values())
        expect_equal(names(d$values()), c("x", "y"))
    })

    test_that("keys are always sorted",
    {
        d <- Dict$new(h = 1, d = 2, a = 8, b = 0)
        expect_false(is.unsorted(d$keys()))

        d$add("c", 3)
        expect_false(is.unsorted(d$keys()))
    })
})


describe("add",
{
    test_that("adding elements to a Dict requires a character key and a value",
    {
        d <- Dict$new()
        expect_error(d$add(1), 'argument "value" is missing')
        expect_error(d$add(name = "a"), 'argument "value" is missing')
        expect_error(d$add("", 1), "name string must not be empty")
        expect_error(
            d$add(1, 1),
            "name must be a character string, but got 'numeric'"
        )
        expect_error(d$add(c("a", "b"), 1:2), "name must be of length 1")
    })

    test_that("adding elements to a Dict requires a character key and a value",
    {
        d <- Dict$new()
        expect_equal(d$add("a", 1), Dict$new(a = 1))
        expect_error(d$add("a", 1), "name 'a' already in Dict")
        expect_error(d$add("a", 2), "name 'a' already in Dict")
        expect_error(d$add("a", NULL), "name 'a' already in Dict")
    })

    test_that("NULL and empty lists can be added",
    {
        d <- Dict$new()

        d$add("empty-list", list())
        d$add("null", NULL)
        expect_equal(d$values(), list("empty-list" = list(), "null" = NULL))
    })
})


describe("at",
{
    d <- Dict$new(a = 1, b = 3)

    it("returns the element at the given positions",
    {
        expect_equal(d$at(1), Dict$new(a = 1))
        expect_equal(d$at(2), Dict$new(b = 3))
        expect_equal(d$at(c("a", "b")), Dict$new(a = 1, b = 3))
        expect_equal(d$at(list(1, "b")), Dict$new(a = 1, b = 3))
        expect_equal(d$at(1:2), Dict$new(a = 1, b = 3))
        expect_equal(d$at("a"), d$at(match("a", names(d))))
        expect_equal(d$at("b"), d$at(match("b", names(d))))
    })

    it("signals errors for invalid indices",
    {
        expect_error(d$at(c(1, 1)), "duplicated keys are not allowed")
        expect_error(d$at(0), "index must be > 0")
        expect_error(d$at(-1), "index must be > 0")
        expect_error(d$at("c"), "index 'c' not found")
        expect_error(d$at(as.numeric(NA)), "index must not be 'NA'")
    })
})


describe("at2",
{
    d <- Dict$new(a = 1, b = 3)

    it("returns the element at the given position",
    {
        expect_equal(d$at2(1), 1)
        expect_equal(d$at2(2), 3)
        expect_equal(d$at2("a"), 1)
    })

    it("signals invalid index",
    {
        expect_error(d$at2(1:2), "index must be of length 1")
        expect_error(d$at2(0), "index must be > 0")
        expect_error(d$at2(-1), "index must be > 0")
        expect_error(d$at2(5), "index 5 exceeds length of Dict, which is 2")
        expect_error(d$at2(as.numeric(NA)), "index must not be 'NA'")
        expect_error(d$at2(c("a", "b")), "index must be of length 1")
        expect_error(d$at2("c"), "index 'c' not found")
    })
})


describe("delete",
{
    it("deletes an element by value as expected",
    {
        d <- Dict$new(a = 1, b = 2, c = 3)
        expect_equal(d$delete(3), Dict$new(a = 1, b = 2))

        d <- Dict$new(a = mean, b = identity)
        expect_equal(d$delete(mean), Dict$new(b = identity))
    })

    it("signals an error if trying to delete a non-existing element",
    {
        d <- Dict$new(a = 1)
        expect_error(d$delete(5), "5 is not in Dict")
        li = list(1, 2)
        expect_error(d$delete(li), "list\\(1, 2\\) is not in Dict")
    })

    it("signals missing arg",
    {
        expect_error(d$delete(), 'argument "elem" is missing, with no default')
    })

    it("if element exists several times, only the first one is deleted",
    {
        d <- Dict$new(a = 1, b = 2, c = 1)
        expect_equal(d$delete(1), Dict$new(b = 2, c = 1))
    })
})


describe("delete_at",
{
    test_that("element can be deleted by position",
    {
        d <- Dict$new(a = 1)
        expect_false(d$delete_at("a")$has_name("a"))
    })

    test_that("if key not in Dict, trying to delete it gives an error",
    {
        d <- Dict$new(a = 1, b = 2)
        expect_error(Dict$new(a = 1)$delete_at("x"), "index 'x' not found")
    })

    test_that("only one key can be deleted at a time",
    {
        d <- Dict$new(a = 1, b = 2)
        expect_error(d$delete_at(c("a", "b")), "index must be of length 1")
    })

    test_that("failed delete does not alter the dict object",
    {
        d <- Dict$new(a = 1, b = 2)
        expect_error(d$delete_at("non-existing"))
        d_was_not_touched <- d$length() == 2
        expect_true(d_was_not_touched)
    })
})


describe("discard",
{
    test_that("discarding something from an empty Dict does not change it",
    {
        expect_equal(Dict$new()$discard(1), Dict$new())
    })

    test_that("elements can be discarded from a Dict",
    {
        d <- Dict$new(a = 1, b = 2, c = 3)
        expect_equal(d$discard(3), Dict$new(a = 1, b = 2))
        expect_equal(d$discard(1), Dict$new(b = 2))

        d <- Dict$new(a = mean, b = identity)
        expect_equal(d$discard(mean), Dict$new(b = identity))
        expect_error(d$discard(), 'argument "elem" is missing, with no default')
    })

    it("is not changed when trying to discard non-existing element",
    {
        d <- Dict$new(a = 1)
        expect_silent(expect_equal(d$discard(5), d))
    })
})


describe("discard_at",
{
    test_that("discarding from an empty Dict does not change it",
    {
        d <- Dict$new()
        expect_equal(Dict$new()$discard_at(1), Dict$new())
    })

    test_that("elements can be discarded by index from a Dict",
    {
        d <- Dict$new(a = 1, b = 2, c = 3)
        expect_equal(d$discard_at(3), Dict$new(a = 1, b = 2))
        expect_equal(d$discard_at("a"), Dict$new(b = 2))

        d <- Dict$new(a = mean, b = identity)
        expect_equal(d$discard_at(1), Dict$new(b = identity))
        expect_error(d$discard_at(), "'index' is missing")
    })

    test_that(
        "Dict is not changed when trying to discard at non-existing index",
    {
        d <- Dict$new(a = 1)
        expect_silent(expect_equal(d$discard_at(5), d))
        expect_silent(expect_equal(d$discard_at(0), d))
        expect_silent(expect_equal(d$discard_at("x"), d))
    })
})


describe("has",
{
    d <- Dict$new(a = 1, b = 2)

    it("can be checked if Dict has a certain key",
    {
        expect_true(d$has_name("a"))
        expect_false(d$has_name("x"))
    })

    it("signals an error if key is not of length 1",
    {
        expect_error(d$has_name(c("a", "b")), "name must be of length 1")
    })
})


describe("is_empty",
{
    it("returns TRUE for an empty Dict and FALSE otherwise",
    {
        expect_true(Dict$new()$is_empty())
        expect_false(Dict$new(a = 1)$is_empty())
        expect_false(Dict$new(a = NULL)$is_empty())
    })
})

describe("keys",
{
    it("returns all keys",
    {
        d <- Dict$new(a = 1, b = 2)
        expect_equal(d$keys(), c("a", "b"))
        d$delete_at("a")
        expect_equal(d$keys(), "b")
    })
})


describe("peek_at",
{
    d <- Dict$new(a = 1, b = 3)

    it("returns the element at given indices",
    {
        expect_equal(d$peek_at(1)$values(), list(a = 1))
        expect_equal(d$peek_at(2)$values(), list(b = 3))
        expect_equal(d$peek_at(1:2)$values(), d$values())
        expect_equal(d$peek_at(c("a", "b"))$values(), d$values())
    })

    it("accepts mixed indices given as a list",
    {
        expect_equal(d$peek_at(list(1, "b"))$values(), d$values())
        expect_equal(d$peek_at(list("a", 2))$values(), d$values())
    })

    it("if no index is specified, returns the whole Dict,
        mimicking the behavior of [] on lists",
    {
        l <- as.list(d)
        expect_equal(l[], l)
        expect_equal(d$peek_at()$values(), d$values())
    })

    it("signals errors for invalid indices",
    {
        expect_error(d$peek_at(c(1, 1)), "duplicated keys are not allowed")
    })

    test_that("if index not found, returns an empty Dict",
    {
        expect_equal(d$peek_at(0), Dict$new())
        expect_equal(d$peek_at(-1), Dict$new())
        expect_equal(d$peek_at("c"), Dict$new())
        expect_equal(d$peek_at(NA_real_), Dict$new())
        expect_equal(d$peek_at(NULL, default = "foo"), Dict$new())
    })

    it("can return a default value for non-existing index if index was
        a string or a named position",
    {
        d$peek_at("z", default = "zvalue") |>
            equals(Dict$new(z = "zvalue")) |>
            expect_true()

        d$peek_at(list("foo" = "z"), default = "zvalue") |>
            equals(Dict$new(z = "zvalue")) |>
            expect_true()

        d$peek_at(list("foo" = 99), default = "zvalue") |>
            equals(Dict$new(foo = "zvalue")) |>
            expect_true()


        d$peek_at(list("s1" = "a", "s2" = "x", "s3" = 9), default = -1) |>
            equals(Dict$new(a = 1, s3 = -1, x = -1)) |>
            expect_true()

        d$peek_at(list(s1 = "a", s2 = "x", s3 = NULL, s4 = 9), default = 0) |>
            equals(Dict$new(a = 1, x = 0, s4 = 0)) |>
            expect_true()

        d$peek_at(c(s1 = "a", s2 = "x"), default = -1) |>
            equals(Dict$new(a = 1, x = -1)) |>
            expect_true()

        d$peek_at(c(s1 = 1, s2 = 2, s3 = 9), default = -1) |>
            equals(Dict$new(s1.a = 1, s2.b = 3, s3 = -1)) |>
            expect_true()

        d$peek_at(c(s1 = "a", s2 = 2, s3 = 9), default = -1) |>
            equals(Dict$new(a = 1, "2" = -1, "9" = -1)) |>
            expect_true()
    })

    test_that("for non-string indices, default value will not work",
    {
        msg <- "all elements must be named"
        expect_error(d$peek_at(0, default = "foo"), msg)
        expect_error(d$peek_at(9, default = "foo"), msg)
        expect_error(d$peek_at(NA, default = "foo"), msg)
    })

    test_that("non-existing indices are ignored",
    {
        expect_true(
            all.equal(d$peek_at(list("a", "x", 9)), Dict$new(a = 1))
        )
        expect_true(
            all.equal(
                d$peek_at(c("a", "x", "z"), default = 0),
                Dict$new(a = 1, x = 0, z = 0)
            )
        )
    })
})


describe("peek_at2",
{
    d <- Dict$new(a = 1, b = 3)

    test_that("returns the element at the given position",
    {
        expect_equal(d$peek_at2(1), 1)
        expect_equal(d$peek_at2(2), 3)
        expect_equal(d$peek_at2("a"), 1)
    })

    test_that("returns NULL if index does not exist",
    {
        expect_equal(Dict$new()$peek_at2(1), NULL)
        expect_equal(Dict$new()$peek_at2("foo"), NULL)
        expect_equal(Dict$new()$peek_at2(NULL), NULL)
        expect_equal(d$peek_at2(), NULL)
        expect_equal(d$peek_at2(-1), NULL)
    })

    test_that("default value can be specified for non-existing index",
    {
        expect_equal(d$peek_at2(1, default = 0), 1)
        expect_equal(d$peek_at2(2, default = 0), 3)
        expect_equal(d$peek_at2("foo", default = 0), 0)
        expect_equal(d$peek_at2(99, default = 0), 0)
        expect_equal(d$peek_at2(0, default = "foo"), "foo")
        expect_equal(d$peek_at2(-1, default = "foo"), "foo")
    })

    test_that("signals invalid index",
    {
        expect_error(d$peek_at2(1:2), "index must be of length 1")
        expect_error(d$peek_at2(letters[1:2]), "index must be of length 1")
        expect_error(d$peek_at2(NA), "index must not be 'NA'")
    })
})


describe("pop",
{
    test_that("elements can be popped by key or position",
    {
        d <- Dict$new(a = 1, b = 2, c = 3)
        expect_equal(d$pop("a"), 1)
        expect_false(d$has("a"))
        expect_equal(d$pop(1), 2)
        expect_false(d$has("b"))
    })

    test_that("if no index specified, pops the last element",
    {
        d <- Dict$new(a = 1, b = 2)
        expect_equal(d$pop(), 2)
        expect_equal(d$pop(), 1)
    })

    test_that("popping non-existent indices gives an error",
    {
        d <- Dict$new(a = 1, b = 2, c = 3)
        expect_error(d$pop("x"), "index 'x' not found")
        expect_error(d$pop(4), "index 4 exceeds length of Dict")
    })

    test_that("pop at empty Dict gives an error",
    {
        expect_error(Dict$new()$pop(), "pop at empty Dict")
    })
})


describe("rename",
{
    test_that("key can be renamed",
    {
        d <- Dict$new(A = 1, B = 2)

        vals <- as.numeric(d$values())
        expect_equal(d$rename("A", "A1")$keys(), c("A1", "B"))

        values_did_not_change <- all(vals %in% as.numeric(d$values()))
        expect_true(values_did_not_change)
    })

    test_that("keys are sorted after renaming",
    {
        d <- Dict$new(A = 1, B = 2)
        expect_equal(d$rename("A", "Z")$keys(), c("B", "Z"))
    })

    test_that("can rename several keys at once",
    {
        d <- Dict$new(A = 1, B = 2)

        # Several keys at once
        d$rename(c("A", "B"), c("x", "y"))
        expect_equal(d$keys(), c("x", "y"))

    })

    test_that("Renaming same key multiple times is possible",
    {
        d <- Dict$new(x = 1, y = 2)

        expect_error(
            d$rename(c("x", "x2"), c("x2", "x3")),
            "Items of 'old' not found in names: 'x2'"
        )
    })

    test_that("keys must be character",
    {
        d <- Dict$new(A = 1, B = 2)
        expect_error(d$rename(1, "C"), "'old' must be character")
        expect_error(d$rename("A", 1), "'new' must be character")
    })

    test_that("signals an error args don't match in length",
    {
        d <- Dict$new(A = 1, B = 2)

        expect_error(
            d$rename("A", c("C", "D")),
            "'old' and 'new' names must be of the same length"
        )
        expect_error(d$rename("A", "B"), "name 'B' already in Dict")
    })

    test_that("prevents renaming to an existing key",
    {
        d <- Dict$new(A = 1, B = 2)
        expect_error(d$rename("A", "B"), "name 'B' already in Dict")
        expect_error(d$rename("B", "A"), "name 'A' already in Dict")
    })

    test_that("signals missing key",
    {
        d <- Dict$new(A = 1, B = 2)

        expect_error(
            d$rename("non-existing", "x"),
            "Items of 'old' not found in names: 'non-existing'"
        )
    })
})


describe("replace",
{
    test_that("can replace elements by value",
    {
        d <- Dict$new(a = 1, b = 2)
        d$replace(1, 3) |> equals(Dict$new(a = 3, b = 2)) |> expect_true()

        d <- Dict$new(a = 1, b = "1")
        d$replace(1, 0) |> equals(Dict$new(a = 0, b = "1")) |> expect_true()
    })

    test_that("works with special elements of basic type",
    {
        d <- Dict$new(a = NULL, b = numeric(0), c = list())
        d$replace(NULL, 0) |>
            equals(Dict$new(a = 0, b = numeric(), c = list())) |>
            expect_true()

        d$replace(numeric(0), 0) |>
            equals(Dict$new(a = 0, b = 0, c = list())) |>
            expect_true()

        d$replace(list(), 0) |>
            equals(Dict$new(a = 0, b = 0, c = 0)) |>
            expect_true()
    })

    test_that("works with non-atomic values",
    {
        d1 <- Dict$new(a = 1, b = "1")
        d2 <- Dict$new(a = 2, b = "2")
        co <- Container$new(NULL)
        d <- Dict$new(d1 = d1, d2 = d2, co = co)
        d$replace(d1, 1) |>
            equals(Dict$new(d1 = 1, d2 = d2, co = co)) |>
            expect_true()

        d$replace(d2, 2) |>
            equals(Dict$new(d1 = 1, d2 = 2, co = co)) |>
            expect_true()

        d$replace(co, 0) |>
            equals(Dict$new(d1 = 1, d2 = 2, co = 0)) |>
            expect_true()
    })

    test_that("signals an error if element is not in Dict",
    {
        d <- Dict$new(a = 99)
        expect_error_fixed(d$replace(0, 1), "old element (0) is not in Dict")

        expect_error_fixed(
            d$replace(NULL, 1),
            "old element (NULL) is not in Dict"
        )
    })

    test_that("add == TRUE is not supported for Dict objects",
    {
        d <- Dict$new(a = 99)
        expect_error(d$replace(0, 1, add = TRUE), "unused argument")
    })
})


describe("replace_at",
{
    test_that("replaces value at given key or position",
    {
        d <- Dict$new(a = 1, b = 2)
        d$replace_at("a", 0) |> equals(Dict$new(a = 0, b = 2)) |> expect_true()
        d$replace_at(1, 9) |> equals(Dict$new(a = 9, b = 2)) |> expect_true()
        d$replace_at("b", 0) |> equals(Dict$new(a = 9, b = 0)) |> expect_true()
        d$replace_at(2, 9) |> equals(Dict$new(a = 9, b = 9)) |> expect_true()
    })

    test_that("can replace by special elements of basic type",
    {
        d <- Dict$new(a = NULL, b = numeric(0))
        d$replace_at("a", integer()) |>
            equals(Dict$new(a = integer(), b = numeric(0))) |>
            expect_true()

        d$replace_at(1, list()) |>
            equals(Dict$new(a = list(), b = numeric(0))) |>
            expect_true()

        d$replace_at(2, NULL) |>
            equals(Dict$new(a = list(), b = NULL)) |>
            expect_true()
    })

    test_that("if add == TRUE element is always added",
    {
        d <- Dict$new()
        expect_equal(d$replace_at("a", 9, add = TRUE), Dict$new(a = 9))
    })

    test_that("signals missing args",
    {
        d <- Dict$new(a = 1, b = 2)

        expect_error(
            d$replace_at(1),
            'argument "value" is missing, with no default'
        )

        expect_error(
            d$replace_at(value = 1),
            "'index' is missing"
        )
    })

    test_that("signals invalid index",
    {
        d <- Dict$new(a = 1, b = 2)

        expect_error(d$replace_at(0, 9), "index must be > 0")
        expect_error(
            d$replace_at(4, 9),
            "index 4 exceeds length of Dict, which is 2"
        )
        expect_error(d$replace_at("x", 9), "index 'x' not found")
        expect_error(d$replace_at(0, 0), "index must be > 0")
    })
})


describe("update",
{
    test_that("a Dict can be updated by another Dict object",
    {
        # nolint start
        d0 <- Dict$new(A = 0)
        d1 <- Dict$new(A = 1, B = 2, C = 12)
        d2 <- Dict$new(              C = 3, D = 4)

        expect_equal(d0$update(d0),         Dict$new(A = 0))
        expect_equal(d0$update(Dict$new()), Dict$new(A = 0))
        expect_equal(Dict$new()$update(d0), Dict$new(A = 0))
        # nolint end

        expect_equal(d1$update(Dict$new()), d1)
        expect_equal(d1$update(d2)$values(), list(A = 1, B = 2, C = 3, D = 4))
        expect_equal(Dict$new()$update(d2), d2)
    })

    test_that(
        "can be updated by another Container object if all elements are named",
    {
        co <- Container$new(A = 1, B = 2)
        expect_equal(d$update(co), Dict$new(A = 1, B = 2))

        co <- Container$new(C = 1, 2)
        expect_error(d$update(co), "all elements of 'other' must be named")
    })
})


describe("clone",
{
    test_that("cloning a Dict works as expected",
    {
        # Since internally, the elements of a Dict are stored in an environment,
        # Dict objects always provide reference semantics when cloned non-deeply
        d1 <- Dict$new(a = 1, b = 2, c = 3)
        d2 <- d1
        dd <- d1$clone()
        dd.deep <- d1$clone(deep = TRUE)
        expect_true(identical(d1, d2))
        expect_false(identical(d1, dd))
        expect_equal(d1$values(), dd$values())
        expect_equal(d1$values(), dd.deep$values())

        d1$delete_at("c")
        expect_true(identical(d1, d2))
        expect_equal(d1$values(), dd$values())
        expect_equal(dd$values(), list(a = 1, b = 2))
        expect_equal(dd.deep$values(), list(a = 1, b = 2, c = 3))
    })

    test_that("Dict objects can be cloned deeply as well",
    {
        d1 <- Dict$new(a = 1)
        d2 <- Dict$new(d1 = d1)

        dd <- d2$clone()
        dd |> equals(Dict$new(d1 = Dict$new(a = 1))) |> expect_true()

        d1$add("b", 2)   # since it was not a deep clone, this will affect dd
        dd |> equals(Dict$new(d1 = Dict$new(a = 1, b = 2))) |> expect_true()

        dd.deep <- d2$clone(deep = TRUE)
        d1$add("c", 3)   # this again affects dd but not dd.deep
        dd |>
            equals(Dict$new(d1 = Dict$new(a = 1, b = 2, c = 3))) |>
            expect_true()

        dd.deep |>
            equals(Dict$new(d1 = Dict$new(a = 1, b = 2))) |>
            expect_true()
    })

    test_that("The deep copy works for double-nested dict",
    {
        d1 <- Dict$new(a = 1)
        d2 <- Dict$new(d1 = d1)
        d3 <- Dict$new(d2 = d2)
        ddd.deep <- d3$clone(deep = TRUE)
        d1$add("d", 4)
        d3 |>
            equals(Dict$new(d2 = Dict$new(d1 = Dict$new(a = 1, d = 4)))) |>
            expect_true()

        ddd.deep |>
            equals(Dict$new(d2 = Dict$new(d1 = Dict$new(a = 1)))) |>
            expect_true()
    })
})
