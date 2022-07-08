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

  population_mye <- read_population_mye(population_mye_path, publication_fin_year)


  # Clean new_data

  new_data <- read_lrt_folder(survey_folder_path) %>%
    dplyr::filter(year == "this_year") %>%
    dplyr::select(name:cons_young) %>%
    #Mutate everything except name and year
    dplyr::mutate(across(c("no_of_vehicles":"cons_young"), as.numeric))


  ##Function to make summary columns for London, England, GB, etc
  summary_cols <- function(data){
    data %>%
      ##Add the date
      dplyr::mutate(`Financial year` = publication_fin_year,
                    London = sum(`Docklands Light Railway`, `London Tramlink`, na.rm = TRUE),
                    `England outside of London` = sum(`Nottingham Express Transit`,
                                                       `Midland Metro`,
                                                       `Sheffield Supertram`,
                                                       `Tyne and Wear Metro`,
                                                       `Manchester Metrolink`,
                                                       `Blackpool Tramway`),

                    England = sum(London, `England outside of London`),
                    GB = sum(England, `Edinburgh Trams`))
    }

  #Instead of a for loop, just pick the table you want
  # LRT0101 Passenger Journeys =========================================================

   lrt0101 <- function(min_tidy_dataset, new_data){
    ##Find our data in the list
    item <- grep("LRT0101", names(min_tidy_dataset))

    #Move new data into wide format
    new <- new_data %>%
      dplyr::select(name, total_boardings) %>%
      ##convert total boardings into millions
      dplyr::mutate(total_boardings = total_boardings/1000000) %>%
      tidyr::spread(name, total_boardings) %>%
      summary_cols()

    min_tidy_dataset[[item]] <- bind_rows(min_tidy_dataset[[item]],
                                          new)
  }

  for (i in 1:length(min_tidy_dataset)){




    # LRT0102 Concessionary Journeys ================================================================================================================================================================

    if (grepl("LRT0102", names(min_tidy_dataset)[[i]], fixed = TRUE)){

      min_tidy_dataset[[i]] <- min_tidy_dataset[[i]] %>%
        tibble::add_row(`Financial year` = publication_fin_year,
                        `Docklands Light Railway` = new_data[new_data$name == "Docklands Light Railway", "cons_boardings"][[1]]/million,
                        `London Tramlink` = new_data[new_data$name == "London Tramlink", "cons_boardings"][[1]]/million,
                        `Nottingham Express Transit` = new_data[new_data$name == "Nottingham Express Transit", "cons_boardings"][[1]]/million,
                        `Midland Metro` = new_data[new_data$name == "Midland Metro", "cons_boardings"][[1]]/million,
                        `Sheffield Supertram` = new_data[new_data$name == "Sheffield Supertram", "cons_boardings"][[1]]/million,
                        `Tyne and Wear Metro` = new_data[new_data$name == "Tyne And Wear Metro", "cons_boardings"][[1]]/million,
                        `Manchester Metrolink` = new_data[new_data$name == "Manchester Metrolink", "cons_boardings"][[1]]/million,
                        `Blackpool Tramway` = new_data[new_data$name == "Blackpool Tramway", "cons_boardings"][[1]]/million,
                        `England outside of London` = `Nottingham Express Transit` + `Midland Metro` + `Sheffield Supertram` + `Tyne and Wear Metro` + `Manchester Metrolink` + `Blackpool Tramway`,
                        England = `Docklands Light Railway` + `London Tramlink` + `England outside of London`)

      message("LRT0102")

    }

    # LRT0103 Passenger Kilometers  =================================================================================================================================================================

    if (grepl("LRT0103", names(min_tidy_dataset)[[i]], fixed = TRUE)){

      min_tidy_dataset[[i]] <- min_tidy_dataset[[i]] %>%
        tibble::add_row(`Financial year` = publication_fin_year,
                        `Docklands Light Railway` = new_data[new_data$name == "Docklands Light Railway", "passenger_km"][[1]]/million,
                        `London Tramlink` = new_data[new_data$name == "London Tramlink", "passenger_km"][[1]]/million,
                        `Nottingham Express Transit` = new_data[new_data$name == "Nottingham Express Transit", "passenger_km"][[1]]/million,
                        `Midland Metro` = new_data[new_data$name == "Midland Metro", "passenger_km"][[1]]/million,
                        `Sheffield Supertram` = new_data[new_data$name == "Sheffield Supertram", "passenger_km"][[1]]/million,
                        `Tyne and Wear Metro` = new_data[new_data$name == "Tyne And Wear Metro", "passenger_km"][[1]]/million,
                        `Manchester Metrolink` = new_data[new_data$name == "Manchester Metrolink", "passenger_km"][[1]]/million,
                        `Blackpool Tramway` = new_data[new_data$name == "Blackpool Tramway", "passenger_km"][[1]]/million,
                        London = `Docklands Light Railway` + `London Tramlink`,
                        `England outside of London` = `Nottingham Express Transit` + `Midland Metro` + `Sheffield Supertram` + `Tyne and Wear Metro` + `Manchester Metrolink` + `Blackpool Tramway`,
                        England = London + `England outside of London`,
                        `Edinburgh Trams` = new_data[new_data$name == "Edinburgh Trams", "passenger_km"][[1]]/million,
                        GB = England + `Edinburgh Trams`,
                        `London underground` = new_data[new_data$name == "London Underground", "passenger_km"][[1]]/million,
                        `Glasgow underground` = new_data[new_data$name == "Glasgow Underground", "passenger_km"][[1]]/million)

      message("LRT0103")

    }

    # LRT0104 Passenger Miles  ======================================================================================================================================================================

    if (grepl("LRT0104", names(min_tidy_dataset)[[i]], fixed = TRUE)){

      min_tidy_dataset[[i]] <- min_tidy_dataset[[i]] %>%
        tibble::add_row(`Financial year` = publication_fin_year,
                        `Docklands Light Railway` = measurements::conv_unit(new_data[new_data$name == "Docklands Light Railway", "passenger_km"][[1]], "km", "mi")/million,
                        `London Tramlink` = measurements::conv_unit(new_data[new_data$name == "London Tramlink", "passenger_km"][[1]], "km", "mi")/million,
                        `Nottingham Express Transit` = measurements::conv_unit(new_data[new_data$name == "Nottingham Express Transit", "passenger_km"][[1]], "km", "mi")/million,
                        `Midland Metro` = measurements::conv_unit(new_data[new_data$name == "Midland Metro", "passenger_km"][[1]], "km", "mi")/million,
                        `Sheffield Supertram` = measurements::conv_unit(new_data[new_data$name == "Sheffield Supertram", "passenger_km"][[1]], "km", "mi")/million,
                        `Tyne and Wear Metro` = measurements::conv_unit(new_data[new_data$name == "Tyne And Wear Metro", "passenger_km"][[1]], "km", "mi")/million,
                        `Manchester Metrolink` = measurements::conv_unit(new_data[new_data$name == "Manchester Metrolink", "passenger_km"][[1]], "km", "mi")/million,
                        `Blackpool Tramway` = measurements::conv_unit(new_data[new_data$name == "Blackpool Tramway", "passenger_km"][[1]], "km", "mi")/million,
                        London = `Docklands Light Railway` + `London Tramlink`,
                        `England outside of London` = `Nottingham Express Transit` + `Midland Metro` + `Sheffield Supertram` + `Tyne and Wear Metro` + `Manchester Metrolink` + `Blackpool Tramway`,
                        England = London + `England outside of London`,
                        `Edinburgh Trams` = measurements::conv_unit(new_data[new_data$name == "Edinburgh Trams", "passenger_km"][[1]], "km", "mi")/million,
                        GB = England + `Edinburgh Trams`,
                        `London underground` = measurements::conv_unit(new_data[new_data$name == "London Underground", "passenger_km"][[1]], "km", "mi")/million,
                        `Glasgow underground` = measurements::conv_unit(new_data[new_data$name == "Glasgow Underground", "passenger_km"][[1]], "km", "mi")/million)

      message("LRT0104")

    }

    # LRT0105 Kilometers Operated  ==================================================================================================================================================================
    # Glasgow underground is divided by 3 because they count per carriage but there are 3 carriages per tram

    if (grepl("LRT0105", names(min_tidy_dataset)[[i]], fixed = TRUE)){

      min_tidy_dataset[[i]] <- min_tidy_dataset[[i]] %>%
        tibble::add_row(`Financial year` = publication_fin_year,
                        `Docklands Light Railway` = new_data[new_data$name == "Docklands Light Railway", "km_operated"][[1]]/million,
                        `London Tramlink` = new_data[new_data$name == "London Tramlink", "km_operated"][[1]]/million,
                        `Nottingham Express Transit` = new_data[new_data$name == "Nottingham Express Transit", "km_operated"][[1]]/million,
                        `Midland Metro` = new_data[new_data$name == "Midland Metro", "km_operated"][[1]]/million,
                        `Sheffield Supertram` = new_data[new_data$name == "Sheffield Supertram", "km_operated"][[1]]/million,
                        `Tyne and Wear Metro` = new_data[new_data$name == "Tyne And Wear Metro", "km_operated"][[1]]/million,
                        `Manchester Metrolink` = new_data[new_data$name == "Manchester Metrolink", "km_operated"][[1]]/million,
                        `Blackpool Tramway` = new_data[new_data$name == "Blackpool Tramway", "km_operated"][[1]]/million,
                        London = `Docklands Light Railway` + `London Tramlink`,
                        `England outside of London` = `Nottingham Express Transit` + `Midland Metro` + `Sheffield Supertram` + `Tyne and Wear Metro` + `Manchester Metrolink` + `Blackpool Tramway`,
                        England = London + `England outside of London`,
                        `Edinburgh Trams` = new_data[new_data$name == "Edinburgh Trams", "km_operated"][[1]]/million,
                        GB = England + `Edinburgh Trams`,
                        `London underground` = new_data[new_data$name == "London Underground", "km_operated"][[1]]/million,
                        `Glasgow underground` = new_data[new_data$name == "Glasgow Underground", "km_operated"][[1]]/(3*million))

      message("LRT0105")

    }

    # LRT0106 Miles Operated ========================================================================================================================================================================
    # Glasgow underground is divided by 3 because they count per carriage but there are 3 carriages per tram

    if (grepl("LRT0106", names(min_tidy_dataset)[[i]], fixed = TRUE)){

      min_tidy_dataset[[i]] <- min_tidy_dataset[[i]] %>%
        tibble::add_row(`Financial year` = publication_fin_year,
                        `Docklands Light Railway` = measurements::conv_unit(new_data[new_data$name == "Docklands Light Railway", "km_operated"][[1]], "km", "mi")/million,
                        `London Tramlink` = measurements::conv_unit(new_data[new_data$name == "London Tramlink", "km_operated"][[1]], "km", "mi")/million,
                        `Nottingham Express Transit` = measurements::conv_unit(new_data[new_data$name == "Nottingham Express Transit", "km_operated"][[1]], "km", "mi")/million,
                        `Midland Metro` = measurements::conv_unit(new_data[new_data$name == "Midland Metro", "km_operated"][[1]], "km", "mi")/million,
                        `Sheffield Supertram` = measurements::conv_unit(new_data[new_data$name == "Sheffield Supertram", "km_operated"][[1]], "km", "mi")/million,
                        `Tyne and Wear Metro` = measurements::conv_unit(new_data[new_data$name == "Tyne And Wear Metro", "km_operated"][[1]], "km", "mi")/million,
                        `Manchester Metrolink` = measurements::conv_unit(new_data[new_data$name == "Manchester Metrolink", "km_operated"][[1]], "km", "mi")/million,
                        `Blackpool Tramway` = measurements::conv_unit(new_data[new_data$name == "Blackpool Tramway", "km_operated"][[1]], "km", "mi")/million,
                        London = `Docklands Light Railway` + `London Tramlink`,
                        `England outside of London` = `Nottingham Express Transit` + `Midland Metro` + `Sheffield Supertram` + `Tyne and Wear Metro` + `Manchester Metrolink` + `Blackpool Tramway`,
                        England = London + `England outside of London`,
                        `Edinburgh Trams` = measurements::conv_unit(new_data[new_data$name == "Edinburgh Trams", "km_operated"][[1]], "km", "mi")/million,
                        GB = England + `Edinburgh Trams`,
                        `London underground` = measurements::conv_unit(new_data[new_data$name == "London Underground", "km_operated"][[1]], "km", "mi")/million,
                        `Glasgow underground` = measurements::conv_unit(new_data[new_data$name == "Glasgow Underground", "km_operated"][[1]], "km", "mi")/(3*million))

      message("LRT0106")

    }

    # LRT0107a Average Length of Journey (km) ==================================================================================================================================================================
    # London, England outside of London and England depend on LRT0101 and LRT0103 being updated already, so don't change their order in the spreadsheet

    if (grepl("LRT0107a", names(min_tidy_dataset)[[i]], fixed = TRUE)){

      min_tidy_dataset[[i]] <- min_tidy_dataset[[i]] %>%
        tibble::add_row(`Financial year` = publication_fin_year,
                        `Docklands Light Railway` = new_data[new_data$name == "Docklands Light Railway", "passenger_km"][[1]]/new_data[new_data$name == "Docklands Light Railway", "total_boardings"][[1]],
                        `London Tramlink` = new_data[new_data$name == "London Tramlink", "passenger_km"][[1]]/new_data[new_data$name == "London Tramlink", "total_boardings"][[1]],
                        `Nottingham Express Transit` = new_data[new_data$name == "Nottingham Express Transit", "passenger_km"][[1]]/new_data[new_data$name == "Nottingham Express Transit", "total_boardings"][[1]],
                        `Midland Metro` = new_data[new_data$name == "Midland Metro", "passenger_km"][[1]]/new_data[new_data$name == "Midland Metro", "total_boardings"][[1]],
                        `Sheffield Supertram` = new_data[new_data$name == "Sheffield Supertram", "passenger_km"][[1]]/new_data[new_data$name == "Sheffield Supertram", "total_boardings"][[1]],
                        `Tyne and Wear Metro` = new_data[new_data$name == "Tyne And Wear Metro", "passenger_km"][[1]]/new_data[new_data$name == "Tyne And Wear Metro", "total_boardings"][[1]],
                        `Manchester Metrolink` = new_data[new_data$name == "Manchester Metrolink", "passenger_km"][[1]]/new_data[new_data$name == "Manchester Metrolink", "total_boardings"][[1]],
                        `Blackpool Tramway` = new_data[new_data$name == "Blackpool Tramway", "passenger_km"][[1]]/new_data[new_data$name == "Blackpool Tramway", "total_boardings"][[1]],
                        London = utils::tail(min_tidy_dataset$LRT0103_passenger_km$London, n=1)/utils::tail(min_tidy_dataset$LRT0101_passenger_journeys$London, n=1),
                        `England outside of London` = utils::tail(min_tidy_dataset$LRT0103_passenger_km$`England outside of London`, n=1)/utils::tail(min_tidy_dataset$LRT0101_passenger_journeys$`England outside of London`, n=1),
                        England = utils::tail(min_tidy_dataset$LRT0103_passenger_km$England, n=1)/utils::tail(min_tidy_dataset$LRT0101_passenger_journeys$England, n=1))

      message("LRT0107a")

    }

    # LRT0107b Average Length of Journey (miles) =======================================================================================================================================================
    # Depends on LRT0107a being updated so don't change their order

    if (grepl("LRT0107b", names(min_tidy_dataset)[[i]], fixed = TRUE)){

      min_tidy_dataset[[i]] <- min_tidy_dataset[[i]] %>%
        tibble::add_row(`Financial year` = publication_fin_year,
                        `Docklands Light Railway` = measurements::conv_unit(utils::tail(min_tidy_dataset$LRT0107a_av_length_of_journey_k$`Docklands Light Railway`, 1), "km", "mi"),
                        `London Tramlink` = measurements::conv_unit(utils::tail(min_tidy_dataset$LRT0107a_av_length_of_journey_k$`London Tramlink`, 1), "km", "mi"),
                        `Nottingham Express Transit` = measurements::conv_unit(utils::tail(min_tidy_dataset$LRT0107a_av_length_of_journey_k$`Nottingham Express Transit`, 1), "km", "mi"),
                        `Midland Metro` = measurements::conv_unit(utils::tail(min_tidy_dataset$LRT0107a_av_length_of_journey_k$`Midland Metro`, 1), "km", "mi"),
                        `Sheffield Supertram` = measurements::conv_unit(utils::tail(min_tidy_dataset$LRT0107a_av_length_of_journey_k$`Sheffield Supertram`, 1), "km", "mi"),
                        `Tyne and Wear Metro` = measurements::conv_unit(utils::tail(min_tidy_dataset$LRT0107a_av_length_of_journey_k$`Tyne and Wear Metro`, 1), "km", "mi"),
                        `Manchester Metrolink` = measurements::conv_unit(utils::tail(min_tidy_dataset$LRT0107a_av_length_of_journey_k$`Manchester Metrolink`, 1), "km", "mi"),
                        `Blackpool Tramway` = measurements::conv_unit(utils::tail(min_tidy_dataset$LRT0107a_av_length_of_journey_k$`Blackpool Tramway`, 1), "km", "mi"),
                        London = measurements::conv_unit(utils::tail(min_tidy_dataset$LRT0107a_av_length_of_journey_k$London, 1), "km", "mi"),
                        `England outside of London` = measurements::conv_unit(utils::tail(min_tidy_dataset$LRT0107a_av_length_of_journey_k$`England outside of London`, 1), "km", "mi"),
                        England = measurements::conv_unit(utils::tail(min_tidy_dataset$LRT0107a_av_length_of_journey_k$England, 1), "km", "mi"))

      message("LRT0107b")

    }

    # LRT0108 Average Occupancy  ======================================================================================================================================================================
    # England outside of London and England depend on LRT0103 and LRT0105 being updated already, so don't change their order in the spreadsheet

    if (grepl("LRT0108", names(min_tidy_dataset)[[i]], fixed = TRUE)){

      min_tidy_dataset[[i]] <- min_tidy_dataset[[i]] %>%
        tibble::add_row(`Financial year` = publication_fin_year,
                        `Docklands Light Railway` = new_data[new_data$name == "Docklands Light Railway", "passenger_km"][[1]]/new_data[new_data$name == "Docklands Light Railway", "km_operated"][[1]],
                        `London Tramlink` = new_data[new_data$name == "London Tramlink", "passenger_km"][[1]]/new_data[new_data$name == "London Tramlink", "km_operated"][[1]],
                        `Nottingham Express Transit` = new_data[new_data$name == "Nottingham Express Transit", "passenger_km"][[1]]/new_data[new_data$name == "Nottingham Express Transit", "km_operated"][[1]],
                        `Midland Metro` = new_data[new_data$name == "Midland Metro", "passenger_km"][[1]]/new_data[new_data$name == "Midland Metro", "km_operated"][[1]],
                        `Sheffield Supertram` = new_data[new_data$name == "Sheffield Supertram", "passenger_km"][[1]]/new_data[new_data$name == "Sheffield Supertram", "km_operated"][[1]],
                        `Tyne and Wear Metro` = new_data[new_data$name == "Tyne And Wear Metro", "passenger_km"][[1]]/new_data[new_data$name == "Tyne And Wear Metro", "km_operated"][[1]],
                        `Manchester Metrolink` = new_data[new_data$name == "Manchester Metrolink", "passenger_km"][[1]]/new_data[new_data$name == "Manchester Metrolink", "km_operated"][[1]],
                        `Blackpool Tramway` = new_data[new_data$name == "Blackpool Tramway", "passenger_km"][[1]]/new_data[new_data$name == "Blackpool Tramway", "km_operated"][[1]],
                        `England outside of London` = utils::tail(min_tidy_dataset$LRT0103_passenger_km$`England outside of London`, n=1)/utils::tail(min_tidy_dataset$LRT0105_vehicle_km$`England outside of London`, n=1),
                        England = utils::tail(min_tidy_dataset$LRT0103_passenger_km$England, n=1)/utils::tail(min_tidy_dataset$LRT0105_vehicle_km$England, n=1))

      message("LRT0108")

    }

    # LRT0109 Passenger Journeys per head  ==================================================================================================================================================================
    # London, England outside of London and England depend on LRT0101 being updated already, so don't change their order in the spreadsheet

    if (grepl("LRT0109", names(min_tidy_dataset)[[i]], fixed = TRUE)){

      min_tidy_dataset[[i]] <- min_tidy_dataset[[i]] %>%
        tibble::add_row(`Financial year` = publication_fin_year,
                        `Docklands Light Railway` = new_data[new_data$name == "Docklands Light Railway", "total_boardings"][[1]]/population_mye$`Docklands Light Railway`[[1]],
                        `London Tramlink` = new_data[new_data$name == "London Tramlink", "total_boardings"][[1]]/population_mye$`London Tramlink`[[1]],
                        `Nottingham Express Transit` = new_data[new_data$name == "Nottingham Express Transit", "total_boardings"][[1]]/population_mye$`Nottingham Express Transit`[[1]],
                        `Midland Metro` = new_data[new_data$name == "Midland Metro", "total_boardings"][[1]]/population_mye$`Midland Metro`[[1]],
                        `Sheffield Supertram` = new_data[new_data$name == "Sheffield Supertram", "total_boardings"][[1]]/population_mye$`Sheffield Supertram`[[1]],
                        `Tyne and Wear Metro` = new_data[new_data$name == "Tyne And Wear Metro", "total_boardings"][[1]]/population_mye$`Tyne And Wear Metro`[[1]],
                        `Manchester Metrolink` = new_data[new_data$name == "Manchester Metrolink", "total_boardings"][[1]]/population_mye$`Manchester Metrolink`[[1]],
                        `Blackpool Tramway` = new_data[new_data$name == "Blackpool Tramway", "total_boardings"][[1]]/population_mye$`Blackpool Tramway`[[1]],
                        London = million*utils::tail(min_tidy_dataset$LRT0101_passenger_journeys$London, n=1)/population_mye$London[[1]],
                        `England outside of London` = million*utils::tail(min_tidy_dataset$LRT0101_passenger_journeys$`England outside of London`, n=1)/population_mye$`England outside London`[[1]],
                        England = million*utils::tail(min_tidy_dataset$LRT0101_passenger_journeys$England, n=1)/population_mye$England[[1]])

      message("LRT0109")

    }

    # LRT0201 Number of Stops ===========================================================================================================================================================================

    if (grepl("LRT0201", names(min_tidy_dataset)[[i]], fixed = TRUE)){

      min_tidy_dataset[[i]] <- min_tidy_dataset[[i]] %>%
        tibble::add_row(`Financial year` = publication_fin_year,
                        `Docklands Light Railway` = new_data[new_data$name == "Docklands Light Railway", "no_of_stops"][[1]],
                        `London Tramlink` = new_data[new_data$name == "London Tramlink", "no_of_stops"][[1]],
                        `Nottingham Express Transit` = new_data[new_data$name == "Nottingham Express Transit", "no_of_stops"][[1]],
                        `Midland Metro` = new_data[new_data$name == "Midland Metro", "no_of_stops"][[1]],
                        `Sheffield Supertram` = new_data[new_data$name == "Sheffield Supertram", "no_of_stops"][[1]],
                        `Tyne and Wear Metro` = new_data[new_data$name == "Tyne And Wear Metro", "no_of_stops"][[1]],
                        `Manchester Metrolink` = new_data[new_data$name == "Manchester Metrolink", "no_of_stops"][[1]],
                        `Blackpool Tramway` = new_data[new_data$name == "Blackpool Tramway", "no_of_stops"][[1]],
                        `England outside of London` = `Nottingham Express Transit` + `Midland Metro` + `Sheffield Supertram` + `Tyne and Wear Metro` + `Manchester Metrolink` + `Blackpool Tramway`,
                        England = `Docklands Light Railway` + `London Tramlink` + `England outside of London`,
                        `Edinburgh Trams` = new_data[new_data$name == "Edinburgh Trams", "no_of_stops"][[1]],
                        GB = England + `Edinburgh Trams`,
                        `London underground` = new_data[new_data$name == "London Underground", "no_of_stops"][[1]],
                        `Glasgow underground` = new_data[new_data$name == "Glasgow Underground", "no_of_stops"][[1]])

      message("LRT0201")

    }

    # LRT0202 Number of Tram Cars ===========================================================================================================================================================================

    if (grepl("LRT0202", names(min_tidy_dataset)[[i]], fixed = TRUE)){

      min_tidy_dataset[[i]] <- min_tidy_dataset[[i]] %>%
        tibble::add_row(`Financial year` = publication_fin_year,
                        `Docklands Light Railway` = new_data[new_data$name == "Docklands Light Railway", "no_of_vehicles"][[1]],
                        `London Tramlink` = new_data[new_data$name == "London Tramlink", "no_of_vehicles"][[1]],
                        `Nottingham Express Transit` = new_data[new_data$name == "Nottingham Express Transit", "no_of_vehicles"][[1]],
                        `Midland Metro` = new_data[new_data$name == "Midland Metro", "no_of_vehicles"][[1]],
                        `Sheffield Supertram` = new_data[new_data$name == "Sheffield Supertram", "no_of_vehicles"][[1]],
                        `Tyne and Wear Metro` = new_data[new_data$name == "Tyne And Wear Metro", "no_of_vehicles"][[1]],
                        `Manchester Metrolink` = new_data[new_data$name == "Manchester Metrolink", "no_of_vehicles"][[1]],
                        `Blackpool Tramway` = new_data[new_data$name == "Blackpool Tramway", "no_of_vehicles"][[1]],
                        London = `Docklands Light Railway` + `London Tramlink`,
                        `England outside of London` = `Nottingham Express Transit` + `Midland Metro` + `Sheffield Supertram` + `Tyne and Wear Metro` + `Manchester Metrolink` + `Blackpool Tramway`,
                        England = London + `England outside of London`,
                        `Edinburgh Trams` = new_data[new_data$name == "Edinburgh Trams", "no_of_vehicles"][[1]],
                        GB = England + `Edinburgh Trams`,
                        `London underground` = new_data[new_data$name == "London Underground", "no_of_vehicles"][[1]],
                        `Glasgow underground` = new_data[new_data$name == "Glasgow Underground", "no_of_vehicles"][[1]])

      message("LRT0202")

    }

    # LRT0203 Route Kilometers ===========================================================================================================================================================================
    # Glasgow is divided by two because they count both directions

    if (grepl("LRT0203", names(min_tidy_dataset)[[i]], fixed = TRUE)){

      min_tidy_dataset[[i]] <- min_tidy_dataset[[i]] %>%
        tibble::add_row(`Financial year` = publication_fin_year,
                        `Docklands Light Railway` = new_data[new_data$name == "Docklands Light Railway", "route_km"][[1]],
                        `London Tramlink` = new_data[new_data$name == "London Tramlink", "route_km"][[1]],
                        `Nottingham Express Transit` = new_data[new_data$name == "Nottingham Express Transit", "route_km"][[1]],
                        `Midland Metro` = new_data[new_data$name == "Midland Metro", "route_km"][[1]],
                        `Sheffield Supertram` = new_data[new_data$name == "Sheffield Supertram", "route_km"][[1]],
                        `Tyne and Wear Metro` = new_data[new_data$name == "Tyne And Wear Metro", "route_km"][[1]],
                        `Manchester Metrolink` = new_data[new_data$name == "Manchester Metrolink", "route_km"][[1]],
                        `Blackpool Tramway` = new_data[new_data$name == "Blackpool Tramway", "route_km"][[1]],
                        `England outside of London` = `Nottingham Express Transit` + `Midland Metro` + `Sheffield Supertram` + `Tyne and Wear Metro` + `Manchester Metrolink` + `Blackpool Tramway`,
                        England = `Docklands Light Railway` + `London Tramlink` + `England outside of London`,
                        `Edinburgh Trams` = new_data[new_data$name == "Edinburgh Trams", "route_km"][[1]],
                        GB = England + `Edinburgh Trams`,
                        `London underground` = new_data[new_data$name == "London Underground", "route_km"][[1]],
                        `Glasgow underground` = new_data[new_data$name == "Glasgow Underground", "route_km"][[1]]/2)

      message("LRT0203")

    }

    # LRT0204 Route Miles ========================================================================================================================================================================
    # Glasgow is divided by two because they count both directions

    if (grepl("LRT0204", names(min_tidy_dataset)[[i]], fixed = TRUE)){

      min_tidy_dataset[[i]] <- min_tidy_dataset[[i]] %>%
        tibble::add_row(`Financial year` = publication_fin_year,
                        `Docklands Light Railway` = measurements::conv_unit(new_data[new_data$name == "Docklands Light Railway", "route_km"][[1]], "km", "mi"),
                        `London Tramlink` = measurements::conv_unit(new_data[new_data$name == "London Tramlink", "route_km"][[1]], "km", "mi"),
                        `Nottingham Express Transit` = measurements::conv_unit(new_data[new_data$name == "Nottingham Express Transit", "route_km"][[1]], "km", "mi"),
                        `Midland Metro` = measurements::conv_unit(new_data[new_data$name == "Midland Metro", "route_km"][[1]], "km", "mi"),
                        `Sheffield Supertram` = measurements::conv_unit(new_data[new_data$name == "Sheffield Supertram", "route_km"][[1]], "km", "mi"),
                        `Tyne and Wear Metro` = measurements::conv_unit(new_data[new_data$name == "Tyne And Wear Metro", "route_km"][[1]], "km", "mi"),
                        `Manchester Metrolink` = measurements::conv_unit(new_data[new_data$name == "Manchester Metrolink", "route_km"][[1]], "km", "mi"),
                        `Blackpool Tramway` = measurements::conv_unit(new_data[new_data$name == "Blackpool Tramway", "route_km"][[1]], "km", "mi"),
                        `England outside of London` = `Nottingham Express Transit` + `Midland Metro` + `Sheffield Supertram` + `Tyne and Wear Metro` + `Manchester Metrolink` + `Blackpool Tramway`,
                        England = `Docklands Light Railway` + `London Tramlink` + `England outside of London`,
                        `Edinburgh Trams` = measurements::conv_unit(new_data[new_data$name == "Edinburgh Trams", "route_km"][[1]], "km", "mi"),
                        GB = England + `Edinburgh Trams`,
                        `London underground` = measurements::conv_unit(new_data[new_data$name == "London Underground", "route_km"][[1]], "km", "mi"),
                        `Glasgow underground` = measurements::conv_unit(new_data[new_data$name == "Glasgow Underground", "route_km"][[1]], "km", "mi")/2)

      message("LRT0204")

    }

    # LRT0301a Passenger Revenue at actual prices ===================================================================================================================================================================

    if (grepl("LRT0301a", names(min_tidy_dataset)[[i]], fixed = TRUE)){

      min_tidy_dataset[[i]] <- min_tidy_dataset[[i]] %>%
        tibble::add_row(`Financial year` = publication_fin_year,
                        `Docklands Light Railway` = rowSums(dplyr::select(new_data[new_data$name == "Docklands Light Railway", ], passenger_receipts:cons_young), na.rm = TRUE)/million,
                        `London Tramlink` = rowSums(dplyr::select(new_data[new_data$name == "London Tramlink", ], passenger_receipts:cons_young), na.rm = TRUE)/million,
                        `Nottingham Express Transit` = rowSums(dplyr::select(new_data[new_data$name == "Nottingham Express Transit", ], passenger_receipts:cons_young), na.rm = TRUE)/million,
                        `Midland Metro` = rowSums(dplyr::select(new_data[new_data$name == "Midland Metro", ], passenger_receipts:cons_young), na.rm = TRUE)/million,
                        `Sheffield Supertram` = rowSums(dplyr::select(new_data[new_data$name == "Sheffield Supertram", ], passenger_receipts:cons_young), na.rm = TRUE)/million,
                        `Tyne and Wear Metro` = rowSums(dplyr::select(new_data[new_data$name == "Tyne And Wear Metro", ], passenger_receipts:cons_young), na.rm = TRUE)/million,
                        `Manchester Metrolink` = rowSums(dplyr::select(new_data[new_data$name == "Manchester Metrolink", ], passenger_receipts:cons_young), na.rm = TRUE)/million,
                        `Blackpool Tramway` = rowSums(dplyr::select(new_data[new_data$name == "Blackpool Tramway", ], passenger_receipts:cons_young), na.rm = TRUE)/million,
                        London = `Docklands Light Railway` + `London Tramlink`,
                        `England outside of London` = `Nottingham Express Transit` + `Midland Metro` + `Sheffield Supertram` + `Tyne and Wear Metro` + `Manchester Metrolink` + `Blackpool Tramway`,
                        England = London + `England outside of London`,
                        `Edinburgh Trams` = rowSums(dplyr::select(new_data[new_data$name == "Edinburgh Trams", ], passenger_receipts:cons_young), na.rm = TRUE)/million,
                        GB = England + `Edinburgh Trams`,
                        `London underground` = rowSums(dplyr::select(new_data[new_data$name == "London Underground", ], passenger_receipts:cons_young), na.rm = TRUE)/million,
                        `Glasgow underground` = rowSums(dplyr::select(new_data[new_data$name == "Glasgow Underground", ], passenger_receipts:cons_young), na.rm = TRUE)/million)

      message("LRT0301a")

    }

    # LRT0301b Passenger Revenue at current year prices ===================================================================================================================================================================
    # Depends on LRT0301a being updated so do not change the order of the sheets

    if (grepl("LRT0301b", names(min_tidy_dataset)[[i]], fixed = TRUE)){

      min_tidy_dataset[[i]] <- min_tidy_dataset$LRT0301a_passenger_rev_actual

      for (j in 1:dplyr::count(min_tidy_dataset$LRT0301a_passenger_rev_actual)[[1]]){

        lrt0301b_row <- min_tidy_dataset$LRT0301a_passenger_rev_actual[j, 2:length(min_tidy_dataset[[i]])[[1]]]
        lrt0301b_row <- lrt0301b_row * gdp_deflator[gdp_deflator$fin_year == min_tidy_dataset[[i]]$`Financial year`[[j]], "relative_deflator"][[1]]

        min_tidy_dataset[[i]][j, 2:length(min_tidy_dataset[[i]])[[1]]] <- lrt0301b_row


      }

      message("LRT0301b")


    }

    # LRT0302a Concessionary Revenue at actual prices ===================================================================================================================================================================

    if (grepl("LRT0302a", names(min_tidy_dataset)[[i]], fixed = TRUE)){

      min_tidy_dataset[[i]] <- min_tidy_dataset[[i]] %>%
        tibble::add_row(`Financial year` = publication_fin_year,
                        `Docklands Light Railway` = rowSums(dplyr::select(new_data[new_data$name == "Docklands Light Railway", ], cons_eld_dis:cons_young), na.rm = TRUE)/million,
                        `London Tramlink` = rowSums(dplyr::select(new_data[new_data$name == "London Tramlink", ], cons_eld_dis:cons_young), na.rm = TRUE)/million,
                        `Nottingham Express Transit` = rowSums(dplyr::select(new_data[new_data$name == "Nottingham Express Transit", ], cons_eld_dis:cons_young), na.rm = TRUE)/million,
                        `Midland Metro` = rowSums(dplyr::select(new_data[new_data$name == "Midland Metro", ], cons_eld_dis:cons_young), na.rm = TRUE)/million,
                        `Sheffield Supertram` = rowSums(dplyr::select(new_data[new_data$name == "Sheffield Supertram", ], cons_eld_dis:cons_young), na.rm = TRUE)/million,
                        `Tyne and Wear Metro` = rowSums(dplyr::select(new_data[new_data$name == "Tyne And Wear Metro", ], cons_eld_dis:cons_young), na.rm = TRUE)/million,
                        `Manchester Metrolink` = rowSums(dplyr::select(new_data[new_data$name == "Manchester Metrolink", ], cons_eld_dis:cons_young), na.rm = TRUE)/million,
                        `Blackpool Tramway` = rowSums(dplyr::select(new_data[new_data$name == "Blackpool Tramway", ], cons_eld_dis:cons_young), na.rm = TRUE)/million,
                        `England outside of London` = `Nottingham Express Transit` + `Midland Metro` + `Sheffield Supertram` + `Tyne and Wear Metro` + `Manchester Metrolink` + `Blackpool Tramway`,
                        England = `Docklands Light Railway` + `London Tramlink` + `England outside of London`)

      message("LRT0302a")

    }

    # LRT0302b Concessionary Revenue at current year prices ===================================================================================================================================================================
    # Depends on LRT0302a being updated so do not change the order of the sheets

    if (grepl("LRT0302b", names(min_tidy_dataset)[[i]], fixed = TRUE)){

      min_tidy_dataset[[i]] <- min_tidy_dataset$LRT0302a_cons_rev_actual

      for (j in 1:dplyr::count(min_tidy_dataset$LRT0302a_cons_rev_actual)[[1]]){

        lrt0302b_row <- min_tidy_dataset$LRT0302a_cons_rev_actual[j, 2:length(min_tidy_dataset[[i]])[[1]]]
        lrt0302b_row <- lrt0302b_row * gdp_deflator[gdp_deflator$fin_year == min_tidy_dataset[[i]]$`Financial year`[[j]], "relative_deflator"][[1]]

        min_tidy_dataset[[i]][j, 2:length(min_tidy_dataset[[i]])[[1]]] <- lrt0302b_row


      }

      message("LRT0302b")

    }

    # population tab ================================================================================================================================================

    if (grepl("population", names(min_tidy_dataset)[[i]], fixed = TRUE)){

      min_tidy_dataset[[i]] <- min_tidy_dataset[[i]] %>%
        tibble::add_row(year_mid = population_mye$year_mid[[1]],
                        `Financial year` = publication_fin_year,
                        `Docklands Light Railway` = population_mye$`Docklands Light Railway`[[1]],
                        `London Tramlink` = population_mye$`London Tramlink`[[1]],
                        `Nottingham Express Transit` = population_mye$`Nottingham Express Transit`[[1]],
                        `Midland Metro` = population_mye$`Midland Metro`[[1]],
                        `Sheffield Supertram` = population_mye$`Sheffield Supertram`[[1]],
                        `Tyne and Wear Metro` = population_mye$`Tyne And Wear Metro`[[1]],
                        `Manchester Metrolink` = population_mye$`Manchester Metrolink`[[1]],
                        `Blackpool Tramway` = population_mye$`Blackpool Tramway`[[1]],
                        London = population_mye$London[[1]],
                        `England outside of London` = population_mye$`England outside London`[[1]],
                        England = population_mye$England[[1]])

      message("Population table")

    }


  }

  # Save updated minimal tidy dataset in specified folder

  # Get year to have in file name, for example 2019/20 file name year is 2020

  file_name_year <- as.integer(strsplit(publication_fin_year, "/")[[1]])
  file_name_year <- as.character(file_name_year[[1]]+1)

  # Save file in directory given

  openxlsx::write.xlsx(min_tidy_dataset, file = paste(save_min_tidy_dataset_path,
                                                      paste(file_name_year, "minimal_tidy_dataset.xlsx", sep = "_"),
                                                      sep = "/"))

}
