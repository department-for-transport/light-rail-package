#' Generates QA note for the light rail and tram publication
#'
#' This function reads the excel files containing all of the relevant survey data from
#' operators. It then generates a QA note and saves it as an html file.
#'
#' @param survey_sent_path is a string containing the path of the survey sent folder. This folder
#' must contain the sent survey forms and nothing else. The path should not end with slashes. For Windows
#' paths, each backslash must be changed to either a forward slash "/" or two backslashes "\\\\".
#'
#' @param survey_response_path is a string containing the path of the survey responses folder. This folder
#' must contain the survey responses and nothing else. The path should not end with slashes. For Windows
#' paths, each backslash must be changed to either a forward slash "/" or two backslashes "\\\\".
#'
#' @param save_QA_note_path is a string containing the path of the folder in which you would
#' like to save the QA note file. The path should not end with slashes. For Windows paths, each
#' backslash must be changed to either a forward slash "/" or two backslashes "\\\\".
#'
#' @param publication_fin_year is a string containing the financial year to which the survey data
#' refers. For example: "2019/20".
#'
#' @param email_response_path is a string containing the path of an excel spreadsheet. The
#' spreadsheet should consist of one "name" column which contains the names of the light rail or
#' tram system (must match the names in the survey forms) and one "comments" column containing the
#' relevant information from the emails. For Windows paths, each backslash must be changed to
#' either a forward slash "/" or two backslashes "\\\\". If there are no email responses to include
#' in the QA note then leave the field blank, the default value is null.
#'
#' @param QA_author is a string containg the name of the author, which will appear on the QA note. If
#' this is not set, the default is "Rose McNally".
#'
#' @param QA_date is a string containing the date which shall appear at the top of the QA note. It should
#' be in the form "15 May 2020". If this is not set, the default is the date on which the QA note is
#' generated.
#'
#' @examples
#' generate_lrt_QA_note("G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/2. Survey set up/Survey forms issued",
#'                      "G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/3. Survey/Received Survey forms",
#'                      "G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/6. Validation",
#'                      "2019/20",
#'                      "G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/3. Survey/Survey response emails.xlsx")
#'

generate_lrt_QA_note <- function(survey_sent_path,
                                 survey_response_path,
                                 save_QA_note_path,
                                 publication_fin_year,
                                 email_response_path = NULL,
                                 QA_author = "Rose McNally",
                                 QA_date = NULL){

  # Check all inputs to make sure they are in correct form

  tramlr::check_path(survey_sent_path)

  tramlr::check_path(survey_response_path)

  tramlr::check_path(save_QA_note_path)

  tramlr::check_financial_year(publication_fin_year)

  if (!is.null(email_response_path)){

    tramlr::check_path(email_response_path)

  }

  tramlr::check_string(QA_author)

  if (!is.null(QA_date)){

    tramlr::check_string(QA_date)

  }



  # Read the survey sent forms and the survey response forms from the excel files =====================


  survey_sent_forms <- tramlr::read_lrt_folder(survey_sent_path)

  survey_response_forms <- tramlr::read_lrt_folder(survey_response_path)

  message("Survey forms loaded")


  # If the email_response_path is not NULL then read the email responses from the excel file, otherwise
  # set email_response_tbl to NULL


  if (!is.null(email_response_path)){

    email_response_tbl <- tramlr::read_email_response(email_response_path)

    message("Email responses loaded")

  } else {

    email_response_tbl <- NULL

  }



  # Record any changes made to the "last year" fields in the response forms ===========================


  sent_response_differences <- tramlr::last_year_check(survey_sent_forms, survey_response_forms)



  # Put data from forms in to separate tables =========================================================


  lrt_QA_params <- tramlr::lrt_make_QA_tables(survey_response_forms)



  # Set the title of the QA note and html file ========================================================


  QA_title <- paste(publication_fin_year, "Light Rail and Tram QA note", sep = " ")

  publication_fin_year_file_name <- gsub("/", "-", publication_fin_year)

  output_file_name <- paste(publication_fin_year_file_name, " Light Rail and Tram QA note", ".html", sep = "")

  rmarkdown_path <- system.file("rmd", "lrt_QA_note.Rmd", package = "tramlr", mustWork = TRUE)



  # Add all the information to the params list ========================================================


  lrt_QA_params[["sent_response_differences_tbl"]] <- sent_response_differences

  lrt_QA_params[["email_response_tbl"]] <- email_response_tbl

  lrt_QA_params[["publication_fin_year"]] <- publication_fin_year

  lrt_QA_params[["QA_author"]] <- QA_author

  lrt_QA_params[["QA_date"]] <- QA_date

  lrt_QA_params[["QA_title"]] <- QA_title



  # Call rmarkdown to render the QA note with specified parameters =====================================


  rmarkdown::render(rmarkdown_path,
                    output_format = "html_document",
                    output_file = output_file_name,
                    output_dir = save_QA_note_path,
                    params = lrt_QA_params)

}
