#' Generates new survey forms
#'
#' This function generates the surveys forms for the next light rail and tram survey
#' and publication.
#'
#' This function takes the blank survey form saved in inst/extdata and substitutes in
#' the correct dates from the arguments. For each operator it fills in the last year
#' figures from last year's validated survey forms (which have the finalised figures in)
#' and saves the excel form in the specified location.
#'
#' @param financial_year is a string containing the financial year to which the survey
#' refers. For example "2020/21".
#'
#' @param survey_deadline is a string containing the date by which the surveys must be
#' returned. It should be in the form "Day Month Year". For example: "5th May 2020".
#'
#' @param last_surveys_folder_path is a string containing the path of the surveys folder for last year.
#' This folder must contain the validated survey forms and nothing else. The path should not end with slashes.
#' For Windows paths, each backslash must be changed to either a forward slash "/" or two backslashes "\\\\".
#'
#' @param save_surveys_path is a string containing the path to the directory in which you
#' would like the survey forms to be saved. A folder with the forms will be created in the
#' directory given. For Windows paths, each backslash must be changed to either a forward slash "/"
#' or two backslashes "\\\\".
#'
#' @examples
#' generate_survey_forms("2020/21",
#'                       "Example date 2021",
#'                       "G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/3. Survey/Validated survey forms",
#'                       "G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2021/2. Survey set up")
#'

generate_survey_forms <- function(financial_year,
                                  survey_deadline,
                                  last_surveys_folder_path,
                                  save_surveys_path){

  # Check inputs ==============================================================================================================================

  tramlr::check_financial_year(financial_year)

  tramlr::check_string(survey_deadline)

  tramlr::check_path(last_surveys_folder_path)

  tramlr::check_path(save_surveys_path)


  # Create directory in specified location if it doesn't exist ================================================================================

  #surveys_folder_name <- paste(financial_year, "Survey forms for issue")

  # Set path of new folder

  surveys_folder_path <- paste(save_surveys_path,
                               "Survey forms for issue",
                               sep = "/")

  # Create folder if it doesn't exist

  if (!dir.exists(surveys_folder_path)){

    dir.create(surveys_folder_path)

  }


  # Load data =================================================================================================================

  last_survey_forms <- tramlr::read_lrt_folder(last_surveys_folder_path)


  # Get dates into correct format to substitute into strings ==================================================================================

  years <- tramlr::get_separate_years(financial_year)


  # Substitute dates into strings

  please_return_sub <- gsub("DATE", survey_deadline, please_return)

  assets_date_sub <- gsub("THIS_YEAR", years[[2]], assets_date)

  section_2_sub <- gsub("LAST_YEAR", years[[1]], section_2)

  section_2_sub <- gsub("THIS_YEAR", years[[2]], section_2_sub)

  services_date_sub <- gsub("LAST_YEAR", years[[1]], services_date)

  services_date_sub <- gsub("THIS_YEAR", years[[2]], services_date_sub)

  please_review_sub <- gsub("FIN_YEAR", financial_year, please_review)


  # Load workbook and add in the strings with the correct dates ==============================================================================

  path_to_blank_survey_form <- system.file("extdata", "blank_survey_form.xlsx", package = "tramlr", mustWork = TRUE)

  wb <- openxlsx::loadWorkbook(path_to_blank_survey_form)

  openxlsx::writeData(wb, sheet = 1, x = please_return_sub, xy = please_return_cell)
  openxlsx::writeData(wb, sheet = 1, x = assets_date_sub, xy = assets_date_cell)
  openxlsx::writeData(wb, sheet = 1, x = section_2_sub, xy = section_2_cell)
  openxlsx::writeData(wb, sheet = 1, x = services_date_sub, xy = services_date_cell)
  openxlsx::writeData(wb, sheet = 1, x = please_review_sub, xy = please_review_cell)


  message("Survey forms generated:")

  for (i in 1:length(all_tram_systems)){

    tramlr::make_survey_form(wb, all_tram_systems[i], financial_year, last_survey_forms, surveys_folder_path)

    message(all_tram_systems[i])

  }

}
