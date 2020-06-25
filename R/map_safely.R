


#' Apply a function to each element of a vector safely
#'
#' Denne funksjonen er en wrapper rundt funksjonene map_dfr og safely fra purrr.
#' Se hjelpeside for purrr::map for doks.
#'
#' @param .x A list or atomic vector
#' @param .f A function, formula, or vector (not necessarily atomic)
#' @param ... .
#' @param .id .
#'
#' @return
#' @export
#'
#' @examples
#' a <- safely_map_dfr(c(-1, 1), ~ data.frame(a = rnorm(.)))
safely_map_dfr <- function(.x, .f, ..., .id = NULL) {

  # .f <- as_mapper(.f, ...) # Tror ikke denne trengs

  .f_safely <- purrr::safely(.f)

  temp <- map(.x, .f_safely)

  res_object <- map_dfr(temp, ~ as.data.frame(.$result))

  err_object <- map(temp, ~ .$error)

  list(result = res_object,
       error = err_object)
}
