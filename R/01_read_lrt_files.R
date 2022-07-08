#' Reads a Light Rail and Tram survey form
#'
#' This function reads a single Light Rail and Tram survey form and returns a
#' tibble containing the data from the form.
#'
#' @param survey_path is a string containing the file path of the survey form.
#' For Windows paths, each backslash must be changed to either a forward slash "/"
#' or two backslashes "\\\\".
#'
#' @return A 2x17 tibble containing all the data entries
#' in the survey form. This does not include the contact details.
#'
#' @examples
#' read_lrt_file("G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/5. RAP Data/Received Survey forms/2019-20 DLR.xlsx")
#'


read_lrt_file <- function(survey_path){


  # Read survey form

  survey_form = readxl::read_excel(survey_path)


  # Extract operator name from survey form

  operator_name <- stringr::str_to_title(survey_form[[1, 1]])


  # Extract data from survey form

  xl_data <- tibble::tibble(
    name               = c(operator_name, operator_name),
    year               = c("this_year", "last_year"),
    no_of_vehicles     = c(survey_form[[cells$no_of_vehicles_row,
                                   cells$this_year_col]],
                           survey_form[[cells$no_of_vehicles_row,
                                   cells$last_year_col]]),
    no_of_stops        = c(survey_form[[cells$no_of_stops_row,
                                   cells$this_year_col]],
                           survey_form[[cells$no_of_stops_row,
                                   cells$last_year_col]]),
    route_km           = c(survey_form[[cells$route_km_row,
                                   cells$this_year_col]],
                           survey_form[[cells$route_km_row,
                                   cells$last_year_col]]),
    km_operated        = c(survey_form[[cells$km_operated_row,
                                   cells$this_year_col]],
                           survey_form[[cells$km_operated_row,
                                   cells$last_year_col]]),
    total_boardings    = c(survey_form[[cells$total_boardings_row,
                                   cells$this_year_col]],
                           survey_form[[cells$total_boardings_row,
                                   cells$last_year_col]]),
    cons_boardings     = c(survey_form[[cells$cons_boardings_row,
                                   cells$this_year_col]],
                           survey_form[[cells$cons_boardings_row,
                                   cells$last_year_col]]),
    passenger_km       = c(survey_form[[cells$passenger_km_row,
                                   cells$this_year_col]],
                           survey_form[[cells$passenger_km_row,
                                   cells$last_year_col]]),
    passenger_receipts = c(survey_form[[cells$passenger_receipts_row,
                                   cells$this_year_col]],
                           survey_form[[cells$passenger_receipts_row,
                                   cells$last_year_col]]),
    cons_eld_dis       = c(survey_form[[cells$cons_eld_dis_row,
                                   cells$this_year_col]],
                           survey_form[[cells$cons_eld_dis_row,
                                   cells$last_year_col]]),
    cons_young         = c(survey_form[[cells$cons_young_row,
                                   cells$this_year_col]],
                           survey_form[[cells$cons_young_row,
                                   cells$last_year_col]]),
    changes_to_fleet   = c(survey_form[[cells$changes_to_fleet_row,
                                   cells$text_col]],
                           NA),
    basis              = c(survey_form[[cells$basis_row,
                                   cells$text_col]],
                           NA),
    source_details     = c(survey_form[[cells$source_details_row,
                                   cells$text_col]],
                           NA),
    description        = c(survey_form[[cells$description_row,
                                   cells$text_col]],
                           NA),
    comments           = c(survey_form[[cells$comments_row,
                                   cells$text_col]],
                           NA)
  )

}




#' Reads all Light Rail and Tram survey forms in folder
#'
#' This function loops through a folder with the light rail and tram survey forms in
#' and returns a tibble containing the data from each survey form.
#'
#' @param survey_folder_path is a string containing the path of the surveys folder. This folder
#' must contain the survey forms and nothing else. The path should not end with slashes. For Windows
#' paths, each backslash must be changed to either a forward slash "/" or two backslashes "\\\\".
#'
#' @return A 2dx17 tibble (where d is the number of survey responses in the folder) containing all
#' the data entries in each survey form. This does not include the contact details.
#'
#' @examples
#' read_lrt_folder("G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/5. RAP Data/Received Survey forms")
#'

read_lrt_folder <- function(survey_folder_path){


  files <- dir(survey_folder_path,
               pattern = ".xlsx",
               full.names = TRUE)

  #Loop over all files in directory
  purrr::map_df(.x = files,
                .f = read_lrt_file)


}




#' Reads email comments file
#'
#' If you wish to include any details given in the email response which were not included
#' in the survey form, this funciton will read an excel file with the details in.
#'
#' @param email_response_path is a string containing the path of an excel spreadsheet. The
#' spreadsheet should consist of one "name" column which contains the names of the light rail or
#' tram system (must match the names in the survey forms) and one "comments" column containing the
#' relevant information from the emails. For Windows paths, each backslash must be changed to
#' either a forward slash "/" or two backslashes "\\\\".
#'
#' @return A tibble containing the email responses.
#'
#' @examples
#' read_email_response("G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/5. RAP Data/Survey response emails.xlsx")
#'
#' @importFrom magrittr %>%

read_email_response <- function(email_response_path){

  email_response <- readxl::read_excel(email_response_path) %>%
    tidyr::drop_na()

}



#' Reads the Minimal Tidy Dataset excel file
#'
#' This function reads the light rail and tram Minimal Tidy Dataset excel file and returns
#' a list of tibbles containing the timeseries data.
#'
#' @param min_tidy_dataset_path is a string containing the file path of the Minimal Tidy Dataset
#' excel file. For Windows paths, each backslash must be changed to either a forward slash "/"
#' or two backslashes "\\\\".
#'
#' @return A list of tibbles containing the light rail and tram time series data
#'
#' @examples
#' read_min_tidy_dataset("G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/7. Minimal Tidy Dataset/2019_minimal_tidy_dataset.xlsx")
#'

read_min_tidy_dataset <- function(min_tidy_dataset_path){

  min_tidy_dataset <- min_tidy_dataset_path %>%
                        readxl::excel_sheets() %>%
                        purrr::set_names() %>%
                        purrr::map(readxl::read_excel, path = min_tidy_dataset_path)
}

#' Reads and tidys the GDP Deflator excel file
#'
#' This function reads the GDP Deflator excel file and returns
#' a tidied tibble containing the useful data.
#'
#' @param gdp_deflator_path is a string containing the file path of the GDP Deflator
#' excel file. For Windows paths, each backslash must be changed to either a forward slash "/"
#' or two backslashes "\\\\".
#'
#' @param publication_fin_year is a string containing the financial year to which the survey data
#' refers. For example: "2019/20".
#'
#' @return A tibble containing the GDP Deflator values from 1983/84 to the publication financial year.
#'
#' @examples
#' read_gdp_deflator("G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/4. Data Entry and Table Compilation/ONS/GDP_Deflators_Qtrly_National_Accounts_March_2020.xlsx",
#'                   "2019/20")
#'

read_gdp_deflator <- function(gdp_deflator_path, publication_fin_year){

  # Import GDP Deflator excel sheet and take first 3 columns

  gdp_deflator <- readxl::read_excel(gdp_deflator_path)[1:3]

  # Rename columns nicely
  names(gdp_deflator) <- c("fin_year", "deflator_value", "percent_change")

  #Clean up FY values to remove footnotes etc and swap hyphens for forward slashes
  gdp_deflator <- gdp_deflator %>%
    dplyr::mutate(fin_year = gsub("(\\d{4}\\-\\d{2}).*", "\\1", fin_year),
                  fin_year = gsub("[-]", "/", fin_year)) %>%
  # Set the gdp deflator value based on the percentage change on previous year
    dplyr::mutate_at(.vars = c("deflator_value", "percent_change"), function(x) as.numeric(x)) %>%
    #Remove NA values
    dplyr::filter(!is.na(percent_change)) %>%
    #Calculate GDP deflator from previous row value and percent change
    dplyr::mutate(deflator_value = dplyr::case_when(!is.na(deflator_value) ~ deflator_value,
                                                    TRUE ~ lag(deflator_value) * (1 + percent_change/100))) %>%
    #Remove percentage change column
    dplyr::select(-percent_change)


  # Keep only rows between first and last values
  gdp_deflator <- gdp_deflator[grep(first_fin_year,
                                    gdp_deflator$fin_year,
                                    fixed = TRUE):
                                 grep(publication_fin_year,
                                      gdp_deflator$fin_year,
                                      fixed = TRUE),]


  # Change hyphens to forward slashes and add third row for relative deflator

  gdp_deflator <- gdp_deflator %>%
    dplyr::mutate(relative_deflator =
                    gdp_deflator[[grep(publication_fin_year,
                                       gdp_deflator$fin_year,
                                       fixed = TRUE), "deflator_value"]] / deflator_value)

  return(gdp_deflator)

}

#' Reads and tidies the population mid-year estimates excel file
#'
#' This function reads the population mid-year estimates excel file and returns
#' a tidied tibble containing the useful data.
#'
#' @param population_mye_path is a string containing the file path of the population mid-year estimates
#' excel file. For Windows paths, each backslash must be changed to either a forward slash "/"
#' or two backslashes "\\\\".
#'
#' @param publication_fin_year is a string containing the financial year to which the survey data
#' refers. For example: "2019/20".
#'
#' @return A tibble containing the population mid-year estimates value for each area for the most recent
#' year in the excel file.
#'
#' @examples
#' read_population_mye("G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/4. Data Entry and Table Compilation/ONS/ukmidyearestimates20182019ladcodes.xls",
#'                     "2019/20")
#'

read_population_mye <- function(population_mye_path, publication_fin_year){

  # Get mid year date, for example 2019/20 mid year date is 2018

  mid_year_date <- as.numeric(gsub("(\\d{4}).*", "\\1", publication_fin_year)) - 1

  # Read MYE 5 tab from excel file

  population_mye_full <- readxl::read_excel(population_mye_path,
                                            "MYE 5",
                                            skip = 4)[1:5]

  # For each code in the area_codes constant, find the population and add it to the tibble
  population_mye <- area_codes %>%
    dplyr::left_join(population_mye_full) %>%
    dplyr::select(area_name, pop = "Estimated Population mid-2018") %>%
    dplyr::mutate(Financial_year = publication_fin_year,
                  year_mid = mid_year_date) %>%
    #Move to wide form
    tidyr::spread(area_name, pop) %>%
    #Create sum columns for England outside London and England total
    dplyr::mutate("England outside London" = sum(`Blackpool Tramway`, `Manchester Metrolink`, `Midland Metro`,
                                                 `Nottingham Express Transit`, `Sheffield Supertram`, `Tyne And Wear Metro`, na.rm = TRUE),
                  "England" = sum(`England outside London`, London))


  return(population_mye)

}

