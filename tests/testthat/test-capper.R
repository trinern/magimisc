# Test of function "capper"

testthat::test_that("Capper function behaves in an orderly fashion", {

  num_vector <- runif(n = 100, min = 1, max = 1000)

  capping <- capper(num_vector, floor = 100, ceiling = 500)

  # Test length of vector
  testthat::expect_equal(length(capping), 100)

  # Test that all numbers are greater than or equal to floor
  purrr::map(capping, ~testthat::expect_gte(.x, 100))

  # Test that all numbers are smaller than or equal to ceiling
  purrr::map(capping, ~testthat::expect_lte(.x, 500))
})
