
#' Discount Cashflow
#'
#'This function discounts a cash flow with the interest and return the net present value (NPV). The function assumes
#'that the first period of \code{df} is period zero in the npv calculation, thus the cash flows in this period is not
#'discounted.
#'
#' @param df Data frame with cash flow, interest rate and period
#' @param cf Cash flow
#' @param rate Interest rate
#' @param period Period, e.g. year.
#'

#'
#' @return
#' @export
#' @import dplyr
#'
#' @examples
#'
#' df <- tibble(
#' cf = c(100, 200, 300),
#' rate = c(0.05, 0.10, 0.20),
#' period = c(2020, 2021, 2022)
#' )
#'
#' df_npv <- discount_cashflow(df, cf, rate, period)
#'
#'
discount_cashflow <- function(df, cf, rate, period) {

  df <- df %>%
    arrange({{period}}) %>%
    mutate(rate_plus_one = if_else({{period}} == min({{period}}), 1, {{rate}} + 1),
           discount = 1 / cumprod(rate_plus_one))

  df_npv <- df %>%
    summarise(npv = sum({{cf}} * discount))
}





