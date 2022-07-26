#' Calculates headline figures for UK, England, London and England outside London
#'
#' This function takes a light rail and tram feature table with details by tram operator and
#' calculates the figures for UK, England, London and England outside London.
#'
#' @param lrt_tbl is a tibble containing the \code{last_year}, \code{this_year}, \code{diff}
#' and \code{percent_change} figures for a given feature (e.g. route_km_tbl) by tram system.
#' The tibble structure should match the tibbles stored in the list output of the
#' \code{\link{lrt_make_all_tables}} function.
#'
#' @return 4x5 tibble containing data for UK, England, London and England outside London
#'
#' @examples
#' calculate_headline_figures(lrt_QA_params$no_of_vehicles_tbl)
#'

calculate_headline_figures <- function(lrt_tbl){

  headline_uk <- tibble::tibble(name = "UK",
                                last_year = sum(lrt_tbl$last_year, na.rm = TRUE),
                                this_year = sum(lrt_tbl$this_year, na.rm = TRUE),
                                diff = this_year - last_year,
                                percent_change = tramlr::round_up((diff / last_year) * 100, digits = 2))

  lrt_tbl_en <- dplyr::filter(lrt_tbl, name %in% trams_in_england)

  headline_en <- tibble::tibble(name = "England",
                                last_year = sum(lrt_tbl_en$last_year, na.rm = TRUE),
                                this_year = sum(lrt_tbl_en$this_year, na.rm = TRUE),
                                diff = this_year - last_year,
                                percent_change = tramlr::round_up((diff / last_year) * 100, digits = 2))

  lrt_tbl_london <- dplyr::filter(lrt_tbl, name %in% trams_in_london)

  headline_london <- tibble::tibble(name = "London",
                                    last_year = sum(lrt_tbl_london$last_year, na.rm = TRUE),
                                    this_year = sum(lrt_tbl_london$this_year, na.rm = TRUE),
                                    diff = this_year - last_year,
                                    percent_change = tramlr::round_up((diff / last_year) * 100, digits = 2))

  lrt_tbl_eol <- dplyr::filter(lrt_tbl, name %in% trams_in_eol)

  headline_eol <- tibble::tibble(name = "England outside London",
                                 last_year = sum(lrt_tbl_eol$last_year, na.rm = TRUE),
                                 this_year = sum(lrt_tbl_eol$this_year, na.rm = TRUE),
                                 diff = this_year - last_year,
                                 percent_change = tramlr::round_up((diff / last_year) * 100, digits = 2))


  headlines <- dplyr::bind_rows(headline_uk,
                         headline_en,
                         headline_london,
                         headline_eol)

}



#' Checks whether a percentage change is an increase or decrease
#'
#' This function takes a row from an light rail and tram feature table, checks whether
#' the percent_change is positive, negative, or equal to zero, and outputs a string
#' describing the change. To be used in rmarkdown to fill in text about change since last year.
#'
#' @param lrt_tbl_row is a row from a light rail and tram feature table.
#'
#' @param previous_fin_year is a string containing the previous financial year, e.g. "2018/19".
#'
#' @param currency boolean, if set to TRUE, the figures will be printed with a pound sign. Default
#' is FALSE.
#'
#' @param commas boolean, if set to TRUE, the figures will be printed with commas. Default
#' is FALSE.
#'
#' @param km boolean, if set to TRUE, the figures will be printed with "km" at the end. Default
#' is FALSE.
#'
#' @return Output is a string.
#'
#' @examples
#' inc_or_dec(lrt_QA_params$no_of_vehicles_tbl[1,])
#' inc_or_dec(lrt_QA_params$cons_eld_dis[6,])
#'
#'

inc_or_dec <- function(lrt_tbl_row, previous_fin_year, currency = FALSE, commas = FALSE, km = FALSE){

  lrt_tbl_row$diff <- abs(lrt_tbl_row$diff)


  # If commas is set to TRUE, format diff with commas

  if (commas == TRUE){

    lrt_tbl_row$diff <- format(lrt_tbl_row$diff, big.mark = ",", trim = TRUE)

    lrt_tbl_row$this_year <- format(lrt_tbl_row$this_year, big.mark = ",", trim = TRUE)

  }


  # If km is set to true, add km to diff

  if (km == TRUE){

    lrt_tbl_row$diff <- paste(lrt_tbl_row$diff, "km", sep = "")

    lrt_tbl_row$this_year <- paste(lrt_tbl_row$this_year, "km", sep = "")

  }


  # If currency is set to TRUE, add £ to diff (\u00A3 is the Unicode encoding)

  if (currency == TRUE){

    lrt_tbl_row$diff <- paste("\u00A3", lrt_tbl_row$diff, sep = "")

    lrt_tbl_row$this_year <- paste("\u00A3", lrt_tbl_row$this_year, sep = "")

  }


  # Format string depending on sign of percentage change

  if (lrt_tbl_row$percent_change > 0){

    return(paste(lrt_tbl_row$this_year,
                 ". This is an increase of ",
                 lrt_tbl_row$diff,
                 " (",
                 signif(lrt_tbl_row$percent_change, digits = 2),
                 "%) compared with ",
                 previous_fin_year,
                 ".",
                 sep = ""))

  } else if (lrt_tbl_row$percent_change < 0){

    return(paste(lrt_tbl_row$this_year,
                 ". This is a decrease of ",
                 lrt_tbl_row$diff,
                 " (",
                 signif(lrt_tbl_row$percent_change, digits = 2),
                 "%) compared with ",
                 previous_fin_year,
                 ".",
                 sep = ""))

  } else if (lrt_tbl_row$percent_change == 0){

    return(paste(lrt_tbl_row$this_year,
                 ". This is unchanged from ",
                 previous_fin_year,
                 ".",
                 sep = ""))

  }

}



#' Gets previous financial year from given financial year
#'
#' This function takes a string of a financial year, e.g. "2019/20" and returns the previous
#' financial year, e.g. "2018/19".
#'
#' @param fin_year is a string of a financial year, in the form "XXXX/XX".
#'
#' @return Output is a string of the previous financial year in the form "XXXX/XX".
#'
#' @examples
#' get_previous_fin_year("2019/20")
#' get_previous_fin_year("2013/14")
#'

get_previous_fin_year <- function(fin_year){

  # Separate string at "/"

  separate_years_str <- strsplit(fin_year, "/")



  # Convert strings to integers

  separate_years_integer <- as.integer(separate_years_str[[1]])



  # Reduce each value by 1

  separate_previous_years_integer <- separate_years_integer - 1



  # Convert back to string

  separate_previous_years_str <- as.character(separate_previous_years_integer)


  # Combine with "/" to one string

  previous_financial_year <- paste(separate_previous_years_str, collapse = "/")

  return(previous_financial_year)

}



#' Checks for NAs and returns message if they are present
#'
#' This function takes a light rail and tram feature table and returns a message if any of
#' the entries are NA.
#'
#' @param lrt_tbl is a tibble containing the \code{last_year}, \code{this_year}, \code{diff}
#' and \code{percent_change} figures for a given feature (e.g. route_km_tbl) by tram system.
#' The tibble structure should match the tibbles stored in the list output of the
#' \code{\link{lrt_make_all_tables}} function.
#'
#' @return Output is a list containing the NA message strings. If there are none, the ouput
#' is an empty list.
#'
#' @examples
#' na_message(passenger_receipts_tbl)
#'

na_message <- function(lrt_tbl){

  na_warnings <- list()

  for (i in 1:dplyr::count(lrt_tbl)[[1]]){

    if (is.na(lrt_tbl[[i,3]])){

      na_string <- paste("Please note:", lrt_tbl$name[[i]], "did not enter a response to this question on the survey.", sep = " ")

      na_warnings[[paste("Warning entry", i, sep = " ")]] <- na_string

    }

  }

  return(na_warnings)

}


