


#' Apply a function to each element of a vector safely
#'
#' Denne funksjonen er en wrapper rundt funksjonene map_dfr og safely fra purrr.
#'
#' Se hjelpeside for purrr::map for doks.
#'
#' @inheritParams purrr::map_dfr
#'
#' @return Funksjonen returnerer et listeobjekt med to elementer.
#' $result inneholder en data.frame med resultat for de elementene som kjørte suksessfullt,
#' mens $error er en liste med samme lengde som .x. Elementene i listen er NULL for de elementene i .x
#' som kjørte uten feil og feilobjektet for de som feilet.
#'
#'
#' @export
#'
#' @examples
#' safely_map_dfr(c(-1, 1), ~ data.frame(a = rnorm(.)))
safely_map_dfr <- function(.x, .f, ..., .id = NULL) {

  # .f <- as_mapper(.f, ...) # Tror ikke denne trengs

  .f_safely <- purrr::safely(.f)

  temp <- purrr::map(.x, .f_safely)

  res_object <- purrr::map_dfr(temp, ~ as.data.frame(.$result))

  err_object <- purrr::map(temp, ~ .$error)

  list(result = res_object,
       error = err_object)
}
