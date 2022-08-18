#' Checks that an input is a string
#'
#' This function checks that the input is a string, and if not, throws an error message
#'
#' @param string is the path inputed in to the function
#'
#' @param string_type string to be inserted in the error message: "The ----- supplied is not a string. Make sure
#' it is surrounded by quotes, and that it is of length 1 (i.e. not a vector).". The default value is "variable".
#'
#' @examples
#' check_string("Hello world")
#'

check_string <- function(string, string_type = "variable"){

  if (!is.character(string) | length(string) > 1){

    not_string <- paste(string, "\n The", string_type, "supplied is not a string. Make sure it is surrounded by quotes, and that it is of length 1 (i.e. not a vector).\n")

    stop(not_string)

  } else {

    return(string)

  }

}


#' Checks path input
#'
#' This function checks that the input path is a string, and that it is referencing
#' a directory that exists. If this is not the case, it throws an error and lets
#' the user know what is wrong.
#'
#' @param path is the path inputed in to the function
#'
#' @examples
#' check_path("G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/5. RAP Data/Received Survey forms/2019-20 DLR.xlsx")
#'

check_path <- function(path){

  tramlr::check_string(path, "path")
  if (!(dir.exists(path) | file.exists(path))){

    not_exist <- paste(path, "/n The directory or file given by this path does not exist")

    stop(not_exist)

  } else {

    return(path)

  }

}


#' Checks financial year input
#'
#' This function checks that the input is a string of the structure matching the financial
#' year format "XXXX/XX".
#'
#' @param financial_year is the input to be tested
#'
#' @examples
#' check_financial_year("2019/20")
#'

check_financial_year <- function(financial_year){


  # Check that the input is a string

  tramlr::check_string(financial_year, "financial year")


  # Check the input contains the correct number of forward slashes

  split_fin_year <- stringr::str_split(financial_year, "/")[[1]]

  if (length(split_fin_year) == 1){

    stop("The financial year given does not include a forward slash. The financial year should be a string of the form \"XXXX/XX\".")

  } else if (length(split_fin_year) > 2){

    stop("The financial year string contains more than one forward slash. The financial year should be a string of the form \"XXXX/XX\".")

  }


  # Check that the number of characters is correct

  if (nchar(financial_year) != 7){

    stop_char_number <- paste("The financial year given has",
                              nchar(financial_year),
                              "characters. It should have 7 characters and be a string of the form \"XXXX/XX\".")

    stop(stop_char_number)

  } else if (!(nchar(split_fin_year[[1]]) == 4 & nchar(split_fin_year[[2]]) == 2)){

    stop_char_arragement <- paste("The number of characters before the forward slash is", nchar(split_fin_year[[1]]), "and the number of characters after the forward slash is", nchar(split_fin_year[[2]]),".",
                                  "There should be 4 before and 2 after, in the form \"XXXX/XX\".")

    stop(stop_char_arragement)

  }


  # Check that the financial year is made up of numbers

  first_part_number <- suppressWarnings(as.integer(split_fin_year[[1]]))

  second_part_number <- suppressWarnings(as.integer(split_fin_year[[2]]))

  if (is.na(first_part_number) | is.na(second_part_number)){

    stop("The financial year string must contain ONLY numbers and a forward slash, in the form \"XXXX/XX\".")

  }


  # Check that the financial year is made up of two consecutive years

  lower_year <- as.integer(substr(split_fin_year[[1]], 3, 4))

  upper_year <- as.integer(split_fin_year[[2]])

  if ((upper_year - lower_year) !=  1){

    stop("The financial year supplied MUST contain two consecutive years. Examples:
         Correct: \"2019/20\"
         Incorrect: \"2019/21\"")

  }

  return(financial_year)

}


#' Checks year gaps input
#'
#' This function checks that the input is an integer between 1 and 12.
#'
#' @param year_gap is the input to be tested
#'
#' @examples
#' check_year_gap(10)
#'

check_year_gap <- function(year_gap){


  # Check that the input is a number

  if (!is.numeric(year_gap)){

    stop("The input must be a number.")

  }


  # Check that the input is a whole number

  if (year_gap %% 1 != 0){

    stop("The input must be a whole number.")

  }


  # Check that the input lies between 1 and 12

  if (!(1 <= year_gap & year_gap <= 12)){

    stop("The input must be between 1 and 12.")

  }

  return(year_gap)

}
