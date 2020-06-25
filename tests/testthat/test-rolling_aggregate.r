# Test of function "rolling_aggregate"

testthat::test_that("Rolling_aggregate function behaves in an orderly fashion", {

  window_size = 5
  ncols = 3
  customers      <- 1:75
  purchase_dates <- seq( as.Date('2016-01-01'),
                         as.Date('2018-12-31'),
                         by=1 )
  n <- 500L

  set.seed(1)

  input_df <- data.table( cust_id   = sample(customers, n, replace=TRUE),
                    date  = sample(purchase_dates, n, replace=TRUE),
                    input_name = sample(500:50000, n, replace=TRUE)/100
  )[, .(input_name = sum(input_name)),
    keyby=.(cust_id, date) ]

  output_df_sum <- rolling_aggregate(input_df, date, window_size, "input_name", "output_name_test", sum, na.rm=TRUE)

  output_df_mean <- rolling_aggregate(input_df, date, window_size, "input_name", "output_name_test", mean, na.rm=TRUE)

  # Test number of rows in dfs
  testthat::expect_equal(nrow(output_df_sum), n-1)
  testthat::expect_equal(nrow(output_df_mean), n-1)

  # Test number of cols in df
  testthat::expect_equal(ncol(output_df_sum), ncols+2)
  testthat::expect_equal(ncol(output_df_mean), ncols+2)
})
