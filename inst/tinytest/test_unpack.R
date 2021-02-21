# ------
# unpack
# ------
expect_equal(unpack(container()), NULL)
expect_equal(unpack(container(NULL)), NULL)
expect_equal(unpack(container(container())), NULL)
expect_equal(unpack(container(container(list()))), NULL)
expect_equal(unpack(list()), NULL)
expect_equal(unpack(list(list())), NULL)
expect_equal(unpack(list(list(1))), 1)
expect_equal(unpack(list(1:2)), 1:2)
expect_equal(unpack(container(numeric())), numeric())
expect_equivalent(unpack(container(data.frame(A = 1:2, B = 3:4))), 1:4)

co <- container(1, mean)
co2 <- container(2, co, co)
expect_equal(unpack(co2), unlist(list(2, as.list(co), as.list(co))))
l <- list(co, list(list(3, 4), co2))
expect_equal(unpack(l), list(1, mean, 3, 4,  2, 1, mean, 1, mean))

co3 <- container(7, 6)
l <- list(list(container(co3,  co3), 0), 1:4)
expect_equal(unpack(l), c(7,6,  7,6,  0,  1:4))


