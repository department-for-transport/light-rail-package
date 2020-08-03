#' Converts the minimal tidy dataset into publication tables in excel
#'
#' This function reads the data in the minimal tidy dataset and converts each table into
#' a fully formatted, publication-ready excel file. The excel files are saved in a new folder
#' in the directory specified in the arguments.
#'
#' @param minimal_tidy_dataset_path is a string containing the file path of the Minimal Tidy Dataset
#' excel file. For Windows paths, each backslash must be changed to either a forward slash "/"
#' or two backslashes "\\\\".
#'
#' @param save_publication_tables_path is a string containing the file path of the folder in which
#' you would like to save the folder of excel files. For Windows paths, each backslash
#' must be changed to either a forward slash "/" or two backslashes "\\\\".
#'
#' @param publication_fin_year is a string containing the financial year to which the survey data
#' refers. For example: "2019/20".
#'
#' @param publication_date is a string containing the date of the publication. For example:
#' "25th June 2020".
#'
#' @examples
#' generate_publication_tables("G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/7. Minimal Tidy Dataset/2020_minimal_tidy_dataset.xlsx",
#'                             "G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0002 Publication/20-05 Light Rail Statistics/Tables",
#'                             "2019/20",
#'                             "25th June 2020")

generate_publication_tables <- function(minimal_tidy_dataset_path,
                                        save_publication_tables_path,
                                        publication_fin_year,
                                        publication_date){

  # Check inputs

  tramlr::check_path(minimal_tidy_dataset_path)

  tramlr::check_path(save_publication_tables_path)

  tramlr::check_financial_year(publication_fin_year)

  tramlr::check_string(publication_date)


  # Create directory in specified location if it doesn't exist ==============================================================================================================================================

  # Get year for publication tables folder name
  # For example, the 2019/20 publication will use the year 2020 in the publication folder name

  folder_name_year <- as.integer(strsplit(publication_fin_year, "/")[[1]])
  folder_name_year <- as.character(folder_name_year[[1]]+1)

  # Set folder name to "YEAR Publication Tables"
  # For example, the 2019/20 publication folder name will be "2020 Publication Tables"

  publication_tables_folder_name <- paste(folder_name_year,
                                          "Publication Tables",
                                          sep = " ")

  # Set path of new folder

  publication_tables_folder_path <- paste(save_publication_tables_path,
                                          publication_tables_folder_name,
                                          sep = "/")

  # Create folder if it doesn't exist

  if (!dir.exists(publication_tables_folder_path)){

    dir.create(publication_tables_folder_path)

  }



  # Get minimal tidy dataset into correct form ==============================================================================================================================================================

  # Load minimal tidy dataset

  minimal_tidy_dataset <- tramlr::read_min_tidy_dataset(minimal_tidy_dataset_path)

  # Drop population table

  minimal_tidy_dataset$population <- NULL


  # Message to user that table generation has started ==============================================================================================================

  message("Tables completed:")


  # Generate tables =========================================================================================================================================================================================

  for (i in 1:length(minimal_tidy_dataset)[[1]]){

    tramlr::make_publication_table(minimal_tidy_dataset[[i]], names(minimal_tidy_dataset)[[i]], publication_tables_folder_path, publication_date)

  }

}

