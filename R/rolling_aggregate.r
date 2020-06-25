#' Add count and aggregated column to data.table
#'
#' @param df data.table
#' @param date Column of format date
#' @param window_size Size of prior window in days
#' @param input_col Input column to aggregate function
#' @param output_name Name of aggregated column
#' @param aggregate_func Aggregate function to be applied to input_col
#' @param ... Arguments to aggregate function
#'
#' @return Original data.table with count and aggregated columns
#' @export
#'
#' @examples
rolling_aggregate <- function(df, date, window_size, input_col, output_name, aggregate_func, ...){
  aggregate_func = rlang::as_function(aggregate_func)
  df[, c("count", output_name) :=
       df[.(cust_id = cust_id, d_dn = date-window_size, d_up = date),
          on=.(cust_id, date >= d_dn, date < d_up),
          .(.N, aggregate_func({{get(input_col)}}, ...))
          , by=.EACHI][, .(N, V2)]
     ]
}
