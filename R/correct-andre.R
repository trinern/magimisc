#' Change Andre or Andrè to André
#'
#' @param x Input vector. Either a character vector, or something coercible to one.
#'
#' @return A character vector.
#' @export
#'
#' @examples
correct_andre <- function(x){
  stringr::str_replace(x, "Andre|Andrè", "André")
}
