#' Capper
#'
#' @description a function that cuts a number between a floor and a ceiling.
#'
#' @param x number or vector to cap
#' @param floor floor of cap
#' @param ceiling ceiling of cap
#'
#' @return a number either equal to x, floor or ceiling, depending on if x is
#' smaller than floor or larger than ceiling, or between floor and ceiling.
#'
#' @export
capper <- function(x, floor, ceiling) {
  pmin(pmax(x, floor), ceiling)
}
