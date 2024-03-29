library(dplyr)

#' Reads a Light Rail and Tram survey form
#'
#' This function reads a single Light Rail and Tram survey form and returns a
#' tibble containing the data from the form.
#'
#' @param survey_path is a string containing the file path of the survey form.
#' For Windows paths, each backslash must be changed to either a forward slash "/"
#' or two backslashes "\\\\".
#'
#' @return A tibble containing all the data entries
#' in the survey form. This does not include the contact details or (currently) comments.
#'
#' @examples
#' read_lrt_file("G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/5. RAP Data/Received Survey forms/2019-20 DLR.xlsx")
#'


read_lrt_file <- function(survey_path){


  # Read survey form

  survey_form = readxl::read_excel(survey_path)


  # Extract data from survey form

  test <- survey_form %>%
    rename(Question = `TRAM AND METRO OPERATOR ANNUAL RETURN [STATS100T]`,
           last_year = `...3`,
           this_year = `...5`) %>%
    select(Question, last_year, this_year) %>%
    mutate(name = survey_form[[1,1]]) %>%
    filter(grepl("[a-zA-Z]{1}\\d{1}", Question)) %>%
    mutate(question_text = case_when(
      Question == "Q1" ~ "no_of_vehicles",
      Question == "Q2" ~ "no_of_stops",
      Question == "Q3" ~ "route_km",
      Question == "Q4" ~ "km_operated",
      Question == "Q5" ~ "total_boardings",
      Question == "Q6" ~ "cons_boardings",
      Question == "Q6a" ~ "cons_eld_dis",
      Question == "Q6b" ~ "cons_young",
      Question == "Q7" ~ "passenger_km",
      Question == "Q8a" ~ "passenger_receipts",
      Question == "Q9" ~ "cons_revenue",
      Question == "Q9a" ~ "cons_revenue_eld_dis",
      Question == "Q9b" ~ "cons_revenue_young",
      Question == "Q8b" ~ "passenger_receipts_la",
      Question == "Q10" ~ "other_revenue",
      Question == "Q11" ~ "total_op_costs",
      Question == "Q11a" ~ "fixed_costs",
      Question == "Q11b" ~ "semi_fixed_costs",
      Question == "Q11c" ~ "variable_costs",

      TRUE ~ "Unsure")) %>%
    filter(question_text != "Unsure") %>%
    select(-Question) %>%
    tidyr::pivot_longer(cols = c("this_year", "last_year"),
                        names_to = "year",
                        values_to= "value") %>%
    tidyr::pivot_wider(names_from = "question_text",
                       values_from = "value")
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
#' @return A 2dx21 tibble (where d is the number of survey responses in the folder) containing all
#' the data entries in each survey form. This does not include the contact details  or (currently) comments.
#'
#' @examples
#' read_lrt_folder("G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/5. RAP Data/Received Survey forms")
#'

read_lrt_folder <- function(survey_folder_path){


  files <- dir(survey_folder_path, pattern = ".xlsx")

  full_data <- NULL

  for (i in 1:length(files)){

    single_data <- read_lrt_file(paste(survey_folder_path, files[i], sep = "/"))

    full_data <- dplyr::bind_rows(full_data, single_data)

  }

  return(full_data)

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

  email_response <- readxl::read_excel(email_response_path)

  email_response %>% tidyr::drop_na()

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

  # Put publication_fin_year and first_fin_year (in constants.R) into format matching
  # financial year column in GDP deflator table

  publication_fin_year_hyphen <- gsub("/", "-", publication_fin_year)

  first_fin_year_hyphen <- gsub("/", "-", first_fin_year)


  # Import GDP Deflator excel sheet and take first 3 columns

  gdp_deflator <- readxl::read_excel(gdp_deflator_path)

  gdp_deflator <- dplyr::select(gdp_deflator, colnames(gdp_deflator)[[1]]:colnames(gdp_deflator)[[3]])


  # For each row, check if first_fin_year or publication_fin_year occur in the first column
  # If a row matches, save the row index

  for (i in 1:dplyr::count(gdp_deflator)[[1]]){

    if (grepl(first_fin_year_hyphen, gdp_deflator[[i,1]], fixed = TRUE)){

      first_row <- i

    } else if (grepl(publication_fin_year_hyphen, gdp_deflator[[i,1]], fixed = TRUE)){

      last_row <- i

    }

  }


  # Remove extra characters from last_row year and set the gdp deflator value based on the
  # percentage change on previous year

  gdp_deflator[[last_row, 1]] <- publication_fin_year_hyphen

  gdp_deflator <- dplyr::mutate_at(gdp_deflator, dplyr::vars(2:3), function(x) as.numeric(x))

  gdp_deflator[[last_row, 2]] <- gdp_deflator[[last_row - 1, 2]] * (1 + gdp_deflator[[last_row, 3]]/hundy_p)


  # Cut table down to years between first and last row inclusive, and remove third column

  gdp_deflator <- gdp_deflator[first_row:last_row, 1:2]

  colnames(gdp_deflator)[[1]] <-  "fin_year"

  colnames(gdp_deflator)[[2]] <- "deflator_value"


  # Change hyphens to forward slashes and add third row for relative deflator

  this_year_deflator <- utils::tail(gdp_deflator$deflator_value, n = 1)

  gdp_deflator$fin_year <- purrr::map_chr(gdp_deflator$fin_year, ~gsub("-", "/", .))

  gdp_deflator <- dplyr::mutate(gdp_deflator, relative_deflator = this_year_deflator / deflator_value)

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

  mid_year_date <- as.integer(strsplit(publication_fin_year, "/")[[1]])

  mid_year_date <- as.character(mid_year_date[[1]]-1)


  # Read MYE 5 tab from excel file

  population_mye_full <- readxl::read_excel(population_mye_path, population_mye_tab)

  population_mye_full <- dplyr::select(population_mye_full, 1:5)


  # Create tibble with just year_mid and financial year

  fin_year <- "Financial year"

  population_mye <- dplyr::tibble(year_mid = mid_year_date,
                                  !!fin_year := publication_fin_year)


  # For each code in the area_codes constant, find the population and add it to the tibble

  for (i in 1:length(area_codes)){

    for (j in 1:dplyr::count(population_mye_full)[[1]]){

      if (grepl(area_codes[[i]][[1]], population_mye_full[[j,1]], fixed = TRUE)){

        name <- area_codes[[i]][[2]]

        pop <- as.integer(population_mye_full[[j,5]])

        population_mye <- tibble::add_column(population_mye, !!name := pop)

      }

    }

  }


  # Add England outside London and England columns

  eol <- "England outside London"

  eol_pop <- population_mye[names(population_mye) %in% trams_in_eol]

  eol_pop_sum <- rowSums(eol_pop[1,])


  population_mye <- dplyr::mutate(population_mye,
                                  !!eol := eol_pop_sum,
                                  England = eol_pop_sum + London)

  return(population_mye)

}

