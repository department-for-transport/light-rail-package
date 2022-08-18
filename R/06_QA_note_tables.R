#' Formats light rail and tram survey data into separate tables for the QA note
#'
#' This function takes the tibble with the data from all the survey responses, and
#' separates it into feature tables; one for each numerical feature, and one for the worded answers.
#' For each numerical feature, it adds a \code{diff} column, which gives the difference between
#' \code{this_year} and \code{last_year}, and a \code{percent_change} column, which gives the percentage
#' change between \code{this_year} and \code{last_year}.
#'
#' @param survey_response is a tibble containing the data from the survey response forms.
#' The tibble structure should match the output of the \code{\link{read_lrt_folder}} function.
#'
#' @return List containing feature tables, one for each numerical feature and one for the worded answers.
#'
#' @examples
#' lrt_make_QA_tables(read_lrt_folder("G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/5. RAP Data/Received Survey forms"))
#'

lrt_make_QA_tables <- function(survey_response){



  # Table for worded answers ------------------------------------------------------------

  words_tbl <- dplyr::select(survey_response, name, year, changes_to_fleet:comments)

  # Filter survey_words to only have answers from "this_year"

  words_tbl <- dplyr::filter(words_tbl, year == "this_year")

  # Remove year column

  words_tbl <- dplyr::select(words_tbl, name, changes_to_fleet:comments)

  # Remove escape sequences and underscores

  words_tbl <- dplyr::mutate_if(words_tbl,
                                is.character,
                                stringr::str_replace_all, pattern = "\r", replacement = "")

  words_tbl <- dplyr::mutate_if(words_tbl,
                                is.character,
                                stringr::str_replace_all, pattern = "\n", replacement = " ")

  words_tbl <- dplyr::mutate_if(words_tbl,
                                is.character,
                                stringr::str_replace_all, pattern = "_", replacement = "")



  # Table for consessionary travel reimbursement for young people ------------------------

  cons_young_tbl <- dplyr::select(survey_response, name, year, cons_young)

  # Filter table to contain only rows with names which match the names in the constant
  # vector cons_young_operators (in constants.R)

  cons_young_tbl <- dplyr::filter(cons_young_tbl, name %in% cons_young_operators)

  cons_young_tbl <- dplyr::mutate_at(cons_young_tbl, dplyr::vars(number_col), function(x) as.numeric(x))

  cons_young_tbl <- tidyr::spread(cons_young_tbl, year, cons_young)

  cons_young_tbl$this_year <- tramlr::round_up(cons_young_tbl$this_year, 2)

  cons_young_tbl$last_year <- tramlr::round_up(cons_young_tbl$last_year, 2)

  cons_young_tbl <- dplyr::mutate(cons_young_tbl,
                                  diff = this_year - last_year,
                                  percent_change = tramlr::round_up((diff / last_year) * 100, digits = 2))




  # Tables for all other numerical answers ----------------------------------------------

  survey_numbers <- dplyr::select(survey_response, name:cons_eld_dis)

  lrt_QA_tables = list()

  for (i in 3:length(survey_numbers)){

    dummy_tibble <- dplyr::select(survey_response, name, year, colnames(survey_response)[[i]])

    dummy_tibble <- dplyr::mutate_at(dummy_tibble, dplyr::vars(number_col), function(x) as.numeric(x))

    dummy_tibble <- tidyr::spread(dummy_tibble, year, colnames(survey_response)[[i]])

    dummy_tibble$this_year <- tramlr::round_up(dummy_tibble$this_year, 2)

    dummy_tibble$last_year <- tramlr::round_up(dummy_tibble$last_year, 2)

    dummy_tibble <- dplyr::mutate(dummy_tibble,
                                  diff = this_year - last_year,
                                  percent_change = tramlr::round_up((diff / last_year) * 100, digits = 2))

    lrt_QA_tables[[paste(colnames(survey_response)[[i]], "tbl", sep = "_")]] <- dummy_tibble

  }

  # Add other tables to list

  lrt_QA_tables[["cons_young_tbl"]] <- cons_young_tbl

  lrt_QA_tables[["words_tbl"]] <- words_tbl

  return(lrt_QA_tables)

}

