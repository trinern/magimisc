#' Capper
#'
#' @description a function that cuts a number between a floor and a ceiling.
#'
#' @param x
#'
#' @param floor
#' @param ceiling
#'
#' @return a number either equal to x, floor or ceiling, depending on if x is
#' smaller than floor or larger than ceiling, or between floor and ceiling.
#'
#' @export
capper <- function(x, floor, ceiling) {
  pmin(pmax(x, floor), ceiling)
}
