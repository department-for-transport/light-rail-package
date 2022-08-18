#' Produces Charts 1-4 for the light rail and tram publication
#'
#' This function reads the data in the minimal tidy dataset, produces the four charts needed for
#' the publication and then saves them as jpegs in the file location given in the arguments.
#'
#' @param minimal_tidy_dataset_path is a string containing the file path of the Minimal Tidy Dataset
#' excel file. For Windows paths, each backslash must be changed to either a forward slash "/"
#' or two backslashes "\\\\".
#'
#' @param save_publication_charts_path is a string containing the file path of the folder in which
#' you would like to save the folder of charts. For Windows paths, each backslash
#' must be changed to either a forward slash "/" or two backslashes "\\\\".
#'
#' @param publication_fin_year is a string containing the financial year to which the survey data
#' refers. For example: "2019/20".
#'
#' @param year_gaps_chart_1 is an integer specifying the difference in years between each x-axis label.
#' For example: the number of years between 1983/84 and 1989/90 is 6. The default is set to 6, the maximum is
#' 12. If the number of years is not easily divisible along the axis, all but the final year will be spaced
#' evenly along the axis.
#'
#' @examples
#' generate_publication_charts("G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/7. Minimal Tidy Dataset/2020_minimal_tidy_dataset.xlsx",
#'                             "G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0002 Publication/20-05 Light Rail Statistics",
#'                             "2019/20")

generate_publication_charts <- function(minimal_tidy_dataset_path,
                                        save_publication_charts_path,
                                        publication_fin_year,
                                        year_gaps_chart_1 = 6){

  # Check inputs

  tramlr::check_path(minimal_tidy_dataset_path)

  tramlr::check_path(save_publication_charts_path)

  tramlr::check_financial_year(publication_fin_year)



  # Create directory in specified location if it doesn't exist ===================================================================================================

  # Get year for publication charts folder name
  # For example, the 2019/20 publication will use the year 2020 in the publication folder name

  folder_name_year <- as.integer(strsplit(publication_fin_year, "/")[[1]])
  folder_name_year <- as.character(folder_name_year[[1]]+1)

  # Set folder name to "YEAR Publication Charts"
  # For example, the 2019/20 publication folder name will be "2020 Publication Charts"

  publication_charts_folder_name <- paste(folder_name_year,
                                          "Publication Charts",
                                          sep = " ")

  # Set path of new folder

  publication_charts_folder_path <- paste(save_publication_charts_path,
                                          publication_charts_folder_name,
                                          sep = "/")

  # Create folder if it doesn't exist

  if (!dir.exists(publication_charts_folder_path)){

    dir.create(publication_charts_folder_path)

  }

  minimal_tidy_dataset <- tramlr::read_min_tidy_dataset(minimal_tidy_dataset_path)

  message("Charts completed:")

  # Chart 1 ======================================================================================================================================================

  # Get data into right format

  passenger_journeys_index <- grep("LRT0101", names(minimal_tidy_dataset))
  passenger_journeys <- dplyr::select(minimal_tidy_dataset[[passenger_journeys_index]], `Financial year`, England)

  # Set circle position

  last_year_chart_1 <- dplyr::count(passenger_journeys)[[1]]
  last_number_chart_1 <- tail(passenger_journeys$England, 1)

  # Work out gaps between years

  int_div <- last_year_chart_1 %/% year_gaps_chart_1
  next_multiple_down <- int_div * year_gaps_chart_1
  axis_labels_chart_1 <- seq(1, next_multiple_down, year_gaps_chart_1)
  axis_labels_chart_1 <- c(axis_labels_chart_1, last_year_chart_1)

  # Get upper y-axis bound - rounds max point up to closest 50

  upper_chart_1 <- max(passenger_journeys$England) / step_chart_1
  upper_chart_1 <- ceiling(upper_chart_1) * step_chart_1

  # Make chart

  chart_1 <- ggplot2::ggplot(data = passenger_journeys, ggplot2::aes(x = `Financial year`, y = England, group = 1)) +
    ggplot2::geom_line(col = light_blue, size = line_size) +
    ggplot2::geom_point(x = last_year_chart_1,
               y = last_number_chart_1,
               shape = circle,
               size = circle_size,
               col = light_blue) +
    ggplot2::labs(x = "",
         y = "") +
    ggplot2::scale_x_discrete(breaks = passenger_journeys$`Financial year`[axis_labels_chart_1]) +
    ggplot2::scale_y_continuous(breaks = seq(lower_chart_1, upper_chart_1, step_chart_1),
                       limits = c(lower_chart_1, upper_chart_1),
                       expand = corner_chart_1) +
    theme_dft

  # Save chart

  chart_1_name <- paste(publication_charts_folder_path, "Chart 1.jpeg", sep = "/")

  ggplot2::ggsave(chart_1_name, chart_1, dpi = chart_dpi, height = chart_height, width = chart_width)

  message("Chart 1")

  # Chart 2 ======================================================================================================================================================

  # Get data into right format

  journeys_per_head_index <- grep("LRT0109", names(minimal_tidy_dataset))
  journeys_per_head <- dplyr::select(minimal_tidy_dataset[[journeys_per_head_index]], `Financial year`, London, England, `England outside of London`)
  journeys_per_head <- utils::tail(journeys_per_head, ten_year_comparison)

  # Set circle positions

  last_number_EOL_chart_2 <- utils::tail(journeys_per_head$`England outside of London`, 1)
  first_number_EOL_chart_2 <- journeys_per_head$`England outside of London`[[1]]
  last_number_LON_chart_2 <- utils::tail(journeys_per_head$London, 1)
  first_number_LON_chart_2 <- journeys_per_head$London[[1]]

  # Get upper and lower y-axis bounds

  upper_chart_2 = ceiling(max(dplyr::select(journeys_per_head, London, England, `England outside of London`)))
  lower_chart_2 = floor(min(dplyr::select(journeys_per_head, London, England, `England outside of London`)))

  # Make chart

  chart_2 <- ggplot2::ggplot(data = journeys_per_head) +
    ggplot2::geom_line(ggplot2::aes(x = `Financial year`, y = England, group = 1), col = light_green, size = line_size, linetype = dashed_line) +
    ggplot2::geom_line(ggplot2::aes(x = `Financial year`, y = London, group = 1), col = orange, size = line_size) +
    ggplot2::geom_line(ggplot2::aes(x = `Financial year`, y = `England outside of London`, group = 1), col = dark_green, size = line_size) +
    ggplot2::geom_point(x = first_year,
               y = first_number_EOL_chart_2,
               shape = circle,
               size = circle_size,
               col = dark_green) +
    ggplot2::geom_point(x = first_year,
               y = first_number_LON_chart_2,
               shape = circle,
               size = circle_size,
               col = orange) +
    ggplot2::geom_point(x = ten_year_comparison,
               y = last_number_EOL_chart_2,
               shape = circle,
               size = circle_size,
               col = dark_green) +
    ggplot2::geom_point(x = ten_year_comparison,
               y = last_number_LON_chart_2,
               shape = circle,
               size = circle_size,
               col = orange) +
    ggplot2::labs(x = "",
         y = "") +
    ggplot2::scale_x_discrete(breaks = journeys_per_head$`Financial year`[seq(1,length(journeys_per_head$`Financial year`), year_gaps_chart_2)]) +
    ggplot2::scale_y_continuous(breaks = seq(lower_chart_2, upper_chart_2, step_chart_2),
                       limits = c(lower_chart_2, upper_chart_2)) +
    theme_dft

  chart_2_name <- paste(publication_charts_folder_path, "Chart 2.jpeg", sep = "/")

  ggplot2::ggsave(chart_2_name, chart_2, dpi = chart_dpi, height = chart_height, width = chart_width)

  message("Chart 2")

  # Chart 3 ======================================================================================================================================================

  # Get data into right format

  passenger_journeys_ten <- utils::tail(passenger_journeys, ten_year_comparison)
  passenger_journeys_div <- dplyr::transmute(passenger_journeys_ten, `Financial year`, England_div = England * 100 / passenger_journeys_ten$England[[1]])

  vehicle_miles_index <- grep("LRT0106", names(minimal_tidy_dataset))
  vehicle_miles <- dplyr::select(minimal_tidy_dataset[[vehicle_miles_index]], `Financial year`, England)
  vehicle_miles <- utils::tail(vehicle_miles, ten_year_comparison)
  vehicle_miles_div <- dplyr::transmute(vehicle_miles, `Financial year`, England_div = England * 100 / vehicle_miles$England[[1]])

  miles_vs_journeys <- tibble::tibble(`Financial year` = vehicle_miles_div$`Financial year`,
                                      vehicle_miles_div = vehicle_miles_div$England_div,
                                      passenger_journeys_div = passenger_journeys_div$England_div)

  # Get upper and lower y-axis bounds

  lower_chart_3 <- min(dplyr::select(miles_vs_journeys, vehicle_miles_div, passenger_journeys_div))
  lower_chart_3 <- floor(lower_chart_3 / step_chart_3) * step_chart_3

  upper_chart_3 <- max(dplyr::select(miles_vs_journeys, vehicle_miles_div, passenger_journeys_div))
  upper_chart_3 <- ceiling(upper_chart_3 / step_chart_3) * step_chart_3

  # Set dashed line position IF 2011/12 is in the chart and is not the first year on the axis,
  # otherwise set it to -1 so it does not appear

  dashed_line_index <- -1

  if (any(grepl(manchester_change_year, miles_vs_journeys$`Financial year`))){

      if (grep(manchester_change_year, miles_vs_journeys$`Financial year`) > 1){

        dashed_line_index <- grep(manchester_change_year, miles_vs_journeys$`Financial year`)

      }

  }

  # Make chart

  chart_3 <- ggplot2::ggplot(data = miles_vs_journeys) +
    ggplot2::geom_line(ggplot2::aes(x = `Financial year`, y = vehicle_miles_div, group = 1), col = light_orange, size = line_size) +
    ggplot2::geom_line(ggplot2::aes(x = `Financial year`, y = passenger_journeys_div, group = 1), col = orange, size = line_size) +
    ggplot2::geom_vline(xintercept = dashed_line_index, linetype = dashed_line, size = dashed_line_size) +
    ggplot2::scale_x_discrete(breaks = miles_vs_journeys$`Financial year`[seq(1,length(miles_vs_journeys$`Financial year`), year_gaps_chart_3)]) +
    ggplot2::scale_y_continuous(breaks = seq(lower_chart_3, upper_chart_3, step_chart_3),
                       limits = c(lower_chart_3, upper_chart_3)) +
    theme_dft

  # Save chart

  chart_3_name <- paste(publication_charts_folder_path, "Chart 3.jpeg", sep = "/")

  ggplot2::ggsave(chart_3_name, chart_3, dpi = chart_dpi, height = chart_3_height, width = chart_3_width)

  message("Chart 3")

  # Chart 4 ======================================================================================================================================================

  # Get data into right format

  concession_revenue_index <- grep("LRT0302a", names(minimal_tidy_dataset))
  concession_revenue <- dplyr::select(minimal_tidy_dataset[[concession_revenue_index]], `Financial year`, England)
  concession_revenue <- utils::tail(concession_revenue, ten_year_comparison)
  concession_revenue_div <- dplyr::transmute(concession_revenue, `Financial year`, England_div = England * 100 / concession_revenue$England[[1]])

  gross_revenue_index <- grep("LRT0301a", names(minimal_tidy_dataset))
  gross_revenue <- dplyr::select(minimal_tidy_dataset[[gross_revenue_index]], `Financial year`, England)
  gross_revenue <- utils::tail(gross_revenue, ten_year_comparison)
  non_concession_revenue <- dplyr::transmute(gross_revenue, `Financial year`, non_con_England = England - concession_revenue$England)
  non_concession_revenue_div <- dplyr::transmute(non_concession_revenue, `Financial year`, England_div = non_con_England * 100 / non_concession_revenue$non_con_England[[1]])



  non_vs_concession <- tibble::tibble(`Financial year` = non_concession_revenue_div$`Financial year`,
                                        non_con_revenue_div = non_concession_revenue_div$England_div,
                                        concession_revenue_div = concession_revenue_div$England_div)

  # Get upper and lower y-axis bounds

  lower_chart_4 <- min(dplyr::select(non_vs_concession, non_con_revenue_div, concession_revenue_div))
  lower_chart_4 <- floor(lower_chart_4 / step_chart_4) * step_chart_4

  upper_chart_4 <- max(dplyr::select(non_vs_concession, non_con_revenue_div, concession_revenue_div))
  upper_chart_4 <- ceiling(upper_chart_4 / step_chart_4) * step_chart_4

  # Make chart

  chart_4 <- ggplot2::ggplot(data = non_vs_concession) +
    ggplot2::geom_line(ggplot2::aes(x = `Financial year`, y = non_con_revenue_div, group = 1), col = dark_green, size = line_size) +
    ggplot2::geom_line(ggplot2::aes(x = `Financial year`, y = concession_revenue_div, group = 1), col = light_green, size = line_size) +
    ggplot2::scale_x_discrete(breaks = non_vs_concession$`Financial year`[seq(1,length(non_vs_concession$`Financial year`), year_gaps_chart_4)]) +
    ggplot2::scale_y_continuous(breaks = seq(lower_chart_4, upper_chart_4, step_chart_4),
                       limits = c(lower_chart_4, upper_chart_4)) +
    theme_dft

  # Save chart

  chart_4_name <- paste(publication_charts_folder_path, "Chart 4.jpeg", sep = "/")

  ggplot2::ggsave(chart_4_name, chart_4, dpi = chart_dpi, height = chart_height, width = chart_width)

  message("Chart 4")


}
