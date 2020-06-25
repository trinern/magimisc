#' Sex from security number
#'
#' @description A function that returns the sex of a individ based on the
#' individs national security number. Rules for national security number:
#' https://no.wikipedia.org/wiki/F%C3%B8dselsnummer
#'  - Any security number must be of lenght 10 or 11
#'  - If female, place 9 number must be even number
#'  - If male, place 9 number must be odd number
#'  - Last two siffers are controls numbers, calculated from the previous numbers.
#'  - First five/six numbers are birth day of individual
#'    - s
#'  - Last 5 numbers are personal numbers for indivudal
#'    - s
#'
#'
#' @param x A character vector of length n >=1
#'
#' @return Sex as character vector of length n with 'M' = males and 'K' = females
#' @export
#'
#' @examples sex_from_security_number(c("30108138382","1108138382"))
sex_from_security_number <- function(x) {
  # Input control
  if (!is.character(x)) x <- as.character(x)

  if (any(nchar(x) < 10) | any(nchar(x) > 11))
    stop(
      paste0('package magimisc: \n
             Number of characters in x must be either 10 or 11. \n',
             'Security number (fodselsnummer) with less than 10 characters are: \n',
             x[nchar(x)<10],
             '\n Fodsnr with more than 11 characters are: \n',
             x[nchar(x)>11]
      )
    )
  # Function
  temp <- nchar(x) == 10
  x[temp] <- paste0("0", x[temp])
  temp <- substr(x, 9, 9) %in% c(1, 3, 5, 7, 9)
  sex <- rep(NA, length(x))
  sex[temp] <- "M"
  sex[!temp] <- "K"

  return(sex)
}

#' Calculating age
#'
#' @description Calculate age from a birthdate to a individual, by the difference
#' between the birthdate and a calculation date.
#'
#' @param birthdate A dateobject
#' @param calculation_date A dateobject
#' @param use_month_decimals logical, return integer or with decimals (in months)
#'
#' @return Numeric vector of integers
#' @importfrom lubridate interval
#' @export
age <- function(birthdate, calculation_date, use_month_decimals = FALSE) {

  # Change format of input arguments to POSIXlt (if they aren't)
  birthdate <- as.POSIXlt(birthdate)
  calculation_date <- as.POSIXlt(calculation_date)

  if(!use_month_decimals) {
    age <- calculation_date$year - birthdate$year

    age <-
      ifelse(
        calculation_date$mon < birthdate$mon |
          (
            calculation_date$mon == birthdate$mon &
              calculation_date$mday <  birthdate$mday
          ),
        age - 1,
        age
      )

    return(as.integer(age))

  } else {
    age_in_months <- interval(birthdate, calculation_date) %/% months(1)
    age_with_decimals <- as.numeric(age_in_months / 12)

    return(age_with_decimals)

  }
}


