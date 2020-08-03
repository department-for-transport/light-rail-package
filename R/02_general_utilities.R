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


  # Split the financial year string at the forward slash

  split_fin_year <- strsplit(financial_year, "/")[[1]]


  # Get each year

  year_1 <- split_fin_year[[1]]

  year_2 <- split_fin_year[[2]]


  # Convert year 2 to an integer

  year_2 <- as.integer(split_fin_year[[2]])


  # Add 2000 to year 2 to get full date

  year_2 <- year_2 + 2000


  # Convert back to string

  year_2 <- as.character(year_2)


  # Put both strings into a vector

  years <- c(year_1, year_2)

  return(years)

}
