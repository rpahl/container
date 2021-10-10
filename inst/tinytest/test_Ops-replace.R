ee = expect_equal

# ----------------------
# Container [<- operator
# ----------------------
co = container(a = 1, b = "bar")
co[1:2] <- 1:2
ee(co, container(a = 1, b = 2))
expect_error(co[3] <- 3, "index out of range")

co[list(1, "b")] <- 3:4
ee(co, container(a = 3, b = 4))

# -----------------------
# Container [[<- operator
# -----------------------
co = container(a = 1, b = "bar")
co[[1]] <- 2
ee(co, container(a = 2, b = "bar"))
co[[2]] <- 9
ee(co, container(a = 2, b = 9))

co[["b"]] <- 0
co[["x"]] <- 0
ee(co, container(a = 2, b = 0, x = 0))
expect_error(co[[4]] <- 1, "index out of range")

co[[{2}]] <- 9
ee(co, container(a = 9, b = 0, x = 0))

co[[{0}]] <- 9
ee(co, container(a = 9, b = 9, x = 0))

expect_error(co[[{ 1 }]] <- 9, "old element \\(1\\) is not in Container")
expect_error(co[[1, 2]] <- 3:4)
expect_error(co[[1:2]] <- 3:4, "index must be of length 1")

# ----------------------
# Container $<- operator
# ----------------------
co = container(a = 1, b = "bar")
co$f <- 3
ee(co, container(a = 1, b = "bar", f = 3))
co$b <- 2
ee(co, container(a = 1, b = 2, f = 3))

co$`x 2` <- 0
ee(co[["x 2"]], 0)


