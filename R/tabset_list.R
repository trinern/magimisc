#' Create tabset from a list
#'
#' @param list A \code{list} of elements to be printed in each tab
#' @param header_level An \code{integer} specifying the level of the headers
#'
#' @return
#' @export
#'
#' @examples
#'
#'
tabset_list <- function(list, header_level) {

  warning("results must be set to 'asis' in chunk options")

  names = names(list)

  if (is.null(names)) {
    stop("cannot create tabsets for a list without names")
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
