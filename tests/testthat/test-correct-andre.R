# Test of function "correct_andre"

testthat::test_that("correct_andre function behaves in an orderly fashion", {

  andre_vector <- c("André", "Andre", "Andrè")

  sindre_vector <- c("Sindré", "Sindre", "Sindrè")

  testthat::expect_equal(correct_andre(andre_vector), rep("André", 3))

  testthat::expect_equal(correct_andre(sindre_vector), sindre_vector)
})
