#' Create tabset from a list
#'
#' Hvert element i listen blir printet i en "tab" i en markdown rapport.
#'
#'
#'
#' @param list A \code{list} of elements to be printed in each tab
#' @param header_level An \code{integer} specifying the level of the headers. Må være en mer enn overskriften over.
#' @param header_names Vektor med navn på tabs.
#' @return
#' @export
#'
#' @examples
#' # The function is meant to be used in a markdown with the
#' # chunk option results="asis".
#' tabset_list(list(a = 1, b = 2), 2)
tabset_list <- function(list, header_level, header_names = NULL) {

  warning("results must be set to 'asis' in chunk options")

  names <- names(list)
  if (is.null(header_names)) {
    list_names <- names(list)
    if (is.null(list_names)) {
      stop("Mangler navn")
      names <- letters[1:length(list)]
    } else {
      names <- list_names
    }
  } else {
    names <- header_names
  }

  hash = rep("#", header_level)
  hash = c(paste0(hash, collapse = ""), " ")

  for (i in 1:length(names)) {
    cat(
      hash,
      names[i],
      "\n \n"
    )
    print(list[[i]])
    cat("\n \n")

  }
}
