#' psum
#'
#' @description function that does rowsums of vectors
#'
#' @param ... list of vectors of equal dimensions
#'
#'
#'
#' @return a vector of equal dimensions as the input vector with the rowsum of input vectors
#'
#'
#' @export
psum <- function(...) {

  as.data.frame(list(...)) %>% rowSums()
}


psum2 <- function(...) {
  as.data.frame(list(...)) %>% rowSums(na.rm = TRUE)
}
