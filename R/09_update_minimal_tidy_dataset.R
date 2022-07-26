#' Updates the minimal tidy dataset and saves it as a new excel file
#'
#' This function updates the minimal tidy dataset with the most recent data from the survey, and then
#' saves the updated data as a new excel file.
#'
#' @param min_tidy_dataset_path is a string containing the file path of the Minimal Tidy Dataset
#' excel file from last year. For Windows paths, each backslash must be changed to either a forward slash "/"
#' or two backslashes "\\\\".
#'
#' @param survey_folder_path is a string containing the path of the surveys folder. This folder
#' must contain the survey forms and nothing else. The path should not end with slashes. For Windows
#' paths, each backslash must be changed to either a forward slash "/" or two backslashes "\\\\".
#'
#' @param gdp_deflator_path is a string containing the file path of the GDP Deflator
#' excel file. For Windows paths, each backslash must be changed to either a forward slash "/"
#' or two backslashes "\\\\".
#'
#' @param population_mye_path is a string containing the file path of the population mid-year estimates
#' excel file. For Windows paths, each backslash must be changed to either a forward slash "/"
#' or two backslashes "\\\\".
#'
#' @param publication_fin_year is a string containing the financial year to which the survey data
#' refers. For example: "2019/20".
#'
#' @param save_min_tidy_dataset_path is a string containing the file path of the folder in which
#' you would like to save the updated minimal tidy dataset file. For Windows paths, each backslash
#' must be changed to either a forward slash "/" or two backslashes "\\\\".
#'
#' @examples
#' update_minimal_tidy_dataset("G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/7. Minimal Tidy Dataset/2019_minimal_tidy_dataset.xlsx",
#'                             "G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/3. Survey/Validated survey forms",
#'                             "G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/4. Data Entry and Table Compilation/ONS/GDP_Deflators_Qtrly_National_Accounts_March_2020.xlsx",
#'                             "G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/4. Data Entry and Table Compilation/ONS/ukmidyearestimates20182019ladcodes.xls",
#'                             "2019/20",
#'                             "G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/7. Minimal Tidy Dataset")
#'

update_minimal_tidy_dataset <- function(min_tidy_dataset_path,
                                        survey_folder_path,
                                        gdp_deflator_path,
                                        population_mye_path,
                                        publication_fin_year,
                                        save_min_tidy_dataset_path){

  # Check inputs
  tramlr::check_path(min_tidy_dataset_path)
  tramlr::check_path(survey_folder_path)
  tramlr::check_path(gdp_deflator_path)
  tramlr::check_path(population_mye_path)
  tramlr::check_financial_year(publication_fin_year)
  tramlr::check_path(save_min_tidy_dataset_path)


  # Read files
  min_tidy_dataset <- read_min_tidy_dataset(min_tidy_dataset_path)
  gdp_deflator <- read_gdp_deflator(gdp_deflator_path, publication_fin_year)
  population_mye <- read_population_mye(population_mye_path)


  # Clean new_data
  new_data <- read_lrt_folder(survey_folder_path) %>%
    dplyr::filter(year == "this_year") %>%
    dplyr::select(name:cons_young) %>%
    #Mutate everything except name and year
    dplyr::mutate(across(c("no_of_vehicles":"cons_young"), as.numeric))

  # Run all of the individual table functions to write to their respective locations
  min_tidy_dataset[[grep("0101", names(min_tidy_dataset))]] <- lrt0101()
  min_tidy_dataset[[grep("0102", names(min_tidy_dataset))]] <- lrt0102()
  min_tidy_dataset[[grep("0103", names(min_tidy_dataset))]] <- lrt0103()
  min_tidy_dataset[[grep("0104", names(min_tidy_dataset))]] <- lrt0104()
  min_tidy_dataset[[grep("0105", names(min_tidy_dataset))]] <- lrt0105()
  min_tidy_dataset[[grep("0106", names(min_tidy_dataset))]] <- lrt0106()
  min_tidy_dataset[[grep("0107a", names(min_tidy_dataset))]] <- lrt0107a()
  min_tidy_dataset[[grep("0107b", names(min_tidy_dataset))]] <- lrt0107b()

  # Get year to have in file name, for example 2019/20 file name year is 2020
  file_name_year <- as.integer(strsplit(publication_fin_year, "/")[[1]])
  file_name_year <- as.character(file_name_year[[1]]+1)

  # Save file in directory given

  openxlsx::write.xlsx(min_tidy_dataset,
                       file = paste0(save_min_tidy_dataset_path, "/",
                                                      file_name_year,
                                                      "_minimal_tidy_dataset.xlsx"))

}
