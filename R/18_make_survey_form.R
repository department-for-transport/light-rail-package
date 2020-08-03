#' Makes a new survey form
#'
#' This function makes a new survey form for next year's survey and publication.
#'
#' @param wb is an openxlsx workbook object, containing the data for the blank excel survey
#' form with the correct dates substituted in.
#'
#' @param operator_name is a string containing the name of the operator to which the survey form
#' ia addressed. It must be the same as that which appears in the minimal tidy dataset headers.
#'
#' @param financial_year is a string containing the financial year to which the survey
#' refers. For example "2020/21".
#'
#' @param last_survey_forms is a tibble containing the data entered into last year's validated
#' survey forms. It must match the output of \code{read_lrt_folder}.
#'
#' @param surveys_folder_path is a string containing the path to the directory in which you
#' would like the survey form to be saved. For Windows paths, each backslash must be changed
#' to either a forward slash "/" or two backslashes "\\\\".
#'
#' @examples
#' make_survey_form(wb,
#'                  "Blackpool Tramway",
#'                  financial_year,
#'                  last_survey_forms,
#'                  surveys_folder_path)
#'

make_survey_form <- function(wb,
                             operator_name,
                             financial_year,
                             last_survey_forms,
                             surveys_folder_path){


  # Substitute operator name in to form ========================================================================================================

  operator_name_upper <- stringr::str_to_upper(operator_name)

  openxlsx::writeData(wb, sheet = 1, x = operator_name_upper, xy = name_cell)

  openxlsx::renameWorksheet(wb, sheet = 1, operator_name)


  # Get last year data from last year survey forms ===========================================================================================

  operator_name_capitalised <- stringr::str_to_title(operator_name_upper)

  last_surveys_row <- dplyr::filter(last_survey_forms, agrepl(operator_name_capitalised, last_survey_forms$name) & year == "this_year")

  number_of_vehicles_value <- as.double(last_surveys_row$no_of_vehicles)

  number_of_stops_value <- as.double(last_surveys_row$no_of_stops)

  route_km_value <- as.double(last_surveys_row$route_km)

  km_operated_value <- as.double(last_surveys_row$km_operated)

  total_boardings_value <- as.double(last_surveys_row$total_boardings)

  concession_boardings_value <- as.double(last_surveys_row$cons_boardings)

  passenger_km_value <- as.double(last_surveys_row$passenger_km)

  gross_receipts_value <- as.double(last_surveys_row$passenger_receipts)

  cons_eld_dis_value <- as.double(last_surveys_row$cons_eld_dis)

  if (operator_name_capitalised %in% cons_young_operators){

    cons_young_value <- as.double(last_surveys_row$cons_young)

  } else {

    cons_young_value <- "NA"

  }


  # Substitute last year values in to excel workbook ===================================================================================================================

  openxlsx::writeData(wb, sheet = 1, x = number_of_vehicles_value, xy = number_of_vehicles_cell)

  openxlsx::writeData(wb, sheet = 1, x = number_of_stops_value, xy = number_of_stops_cell)

  openxlsx::writeData(wb, sheet = 1, x = route_km_value, xy = route_km_cell)

  openxlsx::writeData(wb, sheet = 1, x = km_operated_value, xy = km_operated_cell)

  openxlsx::writeData(wb, sheet = 1, x = total_boardings_value, xy = total_boardings_cell)

  openxlsx::writeData(wb, sheet = 1, x = concession_boardings_value, xy = concession_boardings_cell)

  openxlsx::writeData(wb, sheet = 1, x = passenger_km_value, xy = passenger_km_cell)

  openxlsx::writeData(wb, sheet = 1, x = gross_receipts_value, xy = gross_receipts_cell)

  openxlsx::writeData(wb, sheet = 1, x = cons_eld_dis_value, xy = cons_eld_dis_cell)

  openxlsx::writeData(wb, sheet = 1, x = cons_young_value, xy = cons_young_cell)


  # Save workbook ======================================================================================================================================================

  file_name <- paste(operator_name, "xlsx", sep = ".")

  file_name <- paste(surveys_folder_path, file_name, sep = "/")

  openxlsx::saveWorkbook(wb, file_name, overwrite = TRUE)


}
