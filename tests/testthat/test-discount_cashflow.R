context("Discount Cashflow")

# Make test data
df <- tibble(
  period = c(2020, 2021, 2022, 2023),
  cf = c(100, 200, 400, 500),
  rate = c(0.04, 0.05, 0.03, 0.02)
)

# Solution
df_solution <- df %>%
  arrange(period) %>%
  mutate(rate_plus_one = if_else(period == min(period), 1, rate + 1),
         discount = 1 / cumprod(rate_plus_one))

df_npv_solution <- df_solution %>%
  summarise(npv = sum(cf * discount))

# Test function base scenario

df_npv_test <- discount_cashflow(df, cf, rate, period)

test_that("The function discounts correctly in base scenario", {
  expect_equal(df_npv_solution, df_npv_test)
})


df <- df %>%
  rename(year = period,
         cash_flow = cf,
         interest_rate = rate)

df_npv_test <- discount_cashflow(df = df,
                                 cf = cash_flow,
                                 rate = interest_rate,
                                 period = year)

test_that("The function can handle different variable names", {
  expect_equal(df_npv_solution, df_npv_test)
})



