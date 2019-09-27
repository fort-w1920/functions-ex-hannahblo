library(testthat)

context("Infix: %xor% ")

test_that("%xor% works as expected for scalar input", {
  expect_true(TRUE %xor% FALSE)
  expect_false(TRUE %xor% TRUE)
  expect_true(FALSE %xor% TRUE)
  expect_false(FALSE %xor% FALSE)
})

test_that("%xor% works as expected for vector input", {
  expect_identical(
    c(TRUE, TRUE, FALSE, FALSE) %xor% c(FALSE, TRUE, TRUE, FALSE),
    c(TRUE, FALSE, TRUE, FALSE))
})


