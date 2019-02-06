context("test-ztpois")

test_that("returns valid type when n = 0", {
  expect_type(rztpois(0), "integer")
})

test_that("doesn't return any zeros", {
  expect_false(any(rztpois(1000, lambda = 0.001) == 0))
})
