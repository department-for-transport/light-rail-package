#' Rounding function which rounds .5 up
#'
#' This function rounds numbers as \code{\link{round}} does, except it does not use the
#' round-to-even rule, so 0.5 is rounded to 1, not 0.
#'
#' @param x is the number to be rounded.
#'
#' @param digits is the number of digits to round to. If digits = -1 then it will round to the
#' nearest 10.
#'
#' @examples
#' round_up(2.5, 0)
#'

round_up <- function(x, digits = 0){

  signx <- sign(x)

  z = abs(x) * 10 ^ digits

  z = z + 0.5

  z = trunc(z)

  z = z / (10 ^ digits)

  z = z * signx

  return(z)

}


#' Gets both full years from financial year
#'
#' This function reads the financial year string and returns
#' the two separate years in full, as elements of a vector.
#'
#' @param financial_year is a string of the financial year, in
#' the form "XXXX/XX".
#'
#' @return a vector containing the two years. The structure is
#' c(year_1, year_2).
#'
#' @examples
#' get_separate_years("2019/20")
#'

get_separate_years <- function(financial_year){


  #Keep the first 4 characters of the financial year as a numeric
  year_1 <- as.numeric(gsub("(\\d{4}).*", "\\1", financial_year))

  #Make a vector of that and the subsequent year
  years <- as.character(c(year_1, year_1 + 1))

  return(years)

}

