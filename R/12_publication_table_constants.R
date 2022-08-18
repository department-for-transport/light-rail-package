# This file contains all the information needed to make the publication tables.

# Styles ==========================================================================================================================================================

bottom_border <- openxlsx::createStyle(border = c("bottom"), borderStyle = c("thin"))

dotted_bottom_border <- openxlsx::createStyle(border = c("bottom"), borderStyle = c("dotted"))

LRT_title_style_names <- c("header",
                           "body",
                           "table_title",
                           "title",
                           "title",
                           "title_units")

double_header_body_col_style = c("body_bold_year",
                                 rep("body_right", 10),
                                 "body_bold_right",
                                 "body_right",
                                 "body_right",
                                 "body_bold_right",
                                 rep("body_right", 3))

no_dp_double_header_body_col_style = c("body_bold_year",
                                       rep("body_right_0_dp", 10),
                                       "body_bold_right_0_dp",
                                       "body_right_0_dp",
                                       "body_right_0_dp",
                                       "body_bold_right_0_dp",
                                       rep("body_right_0_dp", 3))

single_header_body_col_style = c("body_bold_year",
                                 rep("body_right", 9),
                                 "body_bold_right")

no_dp_single_header_body_col_style = c("body_bold_year",
                                       rep("body_right_0_dp", 9),
                                       "body_bold_right_0_dp")

# Structure =======================================================================================================================================================

# Positions to add extra columns to improve appearance in double header tables
# Gaps are added after these columns

gap_one <-  "Financial year"

gap_two <- "England"

gap_three <- "GB"

# Column widths

double_header_col_widths = c(12.5,
                             0.5,
                             rep(12.5, 10),
                             2,
                             12.5,
                             12.5,
                             2,
                             12.5,
                             12.5)

single_header_col_widths = rep(12.5, 11)

# Position of first table column

first_table_column <- 1

# Normal row height

normal_row_height <- 14.5

# Year where dotted line is drawn

manchester_line_year_1 <- c("Manchester Metrolink", "2009/10")

manchester_line_year_2 <- c("Manchester Metrolink", "2011/12")

blackpool_line_year <- c("Blackpool Tramway", "2010/11")

midland_line_year <- c("Midland Metro", "2017/18")

# Standard content ================================================================================================================================================

# Hyperlinks

hyperlink_header <- "https://www.gov.uk/government/collections/light-rail-and-tram-statistics"
names(hyperlink_header) <- "Light rail and tram statistics"
class(hyperlink_header) <- "hyperlink"

hyperlink_footer <- "https://www.gov.uk/government/publications/light-rail-and-tram-statistics-guidance"
names(hyperlink_footer) <- "Quality Report"
class(hyperlink_footer) <- "hyperlink"

# Title text

main_title_text = c("Department for Transport Statistics",
                    "Light rail and tram statistics (https://www.gov.uk/government/collections/light-rail-and-tram-statistics)")

# Footer text

source = "Source: DfT Light Rail and Tram Survey"

end_footers <- c("Telephone: 020 7944 3094",
                 "Email: bus.statistics@dft.gov.uk",
                 "Quality Report (https://www.gov.uk/government/publications/light-rail-and-tram-statistics-guidance)",
                 "The figures in this table are National Statistics")

foot_system <- "For further information on these systems including infrastructure changes that may affect the figures, please refer to the Quality Report."

foot_manchester_1 <- "Manchester Metrolink have revised their method for calculation of passenger boardings so the figures from 2010/11 are not directly comparable with previous years."

foot_manchester_2 <- "Figures for Manchester Metrolink represent total mileage of each tram 'set'.  Where two sets are joined to form one train, the  kilometres run will therefore be counted twice.  Based on information supplied by the operator, this affects approximately 7% of services to 2012, around 12% in 11/12 and 12/13 and 20% in 13/14, meaning that figures for later years are not directly comparable with earlier ones (or with other systems).  We estimate that the increasing use of double sets to form trains contributes around a third of the overall increase in vehicle mileage shown for this system since 2011/12."

foot_manchester_3 <- "Figures for Manchester Metrolink are on a different basis to other systems, and this affects the trend shown.  See footnotes to table LRT0105 for for details."

foot_cons <- "Includes journeys made by elderly/disabled passengers, and youth concessionary passengers in areas where they exist."

foot_blackpool_1 <- "1983/84 to 1998/99 Blackpool Tramway data are imputed. The figures use passenger journeys data and an assumed average distance."

foot_blackpool_2 <- "Blackpool Tramway: The number of stops has been shown for one direction of the route (as is the case with the other systems).  In publications prior to 2011/12 the figures shown covered both directions. \nAdditionally, in 2012/13 Blackpool Tramway had 37 stops on the outward journey and 36 stops on the inward journey, as Fleetwood Ferry only had one platform."

foot_midland_2 <- "Midland Metro changed operator in June 2018. For this reason, figures from 2018/19 onwards may not be directly comparable with previous years."

foot_av_occupancy <- "Average vehicle occupancy is calculated by dividing passenger miles by vehicle miles to estimate persons per vehicle."

foot_journeys_per_head <- "Passenger journeys per head was calculated as passenger journeys divided by the number of people in the respective PTEs/ higher tier authority."

foot_inflation_1 <- "These figures are not adjusted for inflation."

foot_inflation_2 <- "Adjusted for inflation using the GDP market price deflator (as at 31 March YEAR)."

foot_DLR_1 <- "The figures for Docklands Light Railway from 2011/12 to 2018/19 have been revised."

foot_DLR_2 <- "The figures for Docklands Light Railway for the financial years 2012/13, 2016/17 and 2017/18 have been revised."

foot_glasgow_1 <- "The figures for Glasgow underground from 2005/06 to 2018/19 have been revised."


# Long footers

long_footers <- list(foot_manchester_2 = 3,
                     foot_blackpool_2 = 2)

# Individual table contents =======================================================================================================================================


table_content <- list(LRT0101 = list(decimal_points = 1,
                                     double_header = TRUE,
                                     London = TRUE,
                                     dotted_line = c(manchester_line_year_1,
                                                     midland_line_year),
                                     inflation = FALSE,
                                     title_text = c("Table LRT0101",
                                                    "Passenger journeys on light rail and trams and undergrounds by system: Great Britain - annual from 1983/84"),
                                     units = "Millions",
                                     footer_text = c(foot_system,
                                                     foot_manchester_1,
                                                     foot_midland_2)),
                      LRT0102 = list(decimal_points = 1,
                                     double_header = FALSE,
                                     London = FALSE,
                                     dotted_line = c(manchester_line_year_1,
                                                     midland_line_year),
                                     inflation = FALSE,
                                     title_text = c("Table LRT0102",
                                                    "Concessionary passenger journeys on light rail and trams by system: England - annual from 2009/10"),
                                     units = "Millions",
                                     footer_text = c(foot_cons,
                                                     foot_system,
                                                     foot_manchester_1,
                                                     foot_midland_2)),
                      LRT0103 = list(decimal_points = 1,
                                     double_header = TRUE,
                                     London = TRUE,
                                     dotted_line = c(manchester_line_year_1,
                                                     midland_line_year),
                                     inflation = FALSE,
                                     title_text = c("Table LRT0103",
                                                    "Passenger kilometres on light rail and trams and undergrounds by system: Great Britain - annual from 1983/84"),
                                     units = "Millions",
                                     footer_text = c(foot_system,
                                                     foot_manchester_1,
                                                     foot_blackpool_1,
                                                     foot_midland_2)),
                      LRT0104 = list(decimal_points = 1,
                                     double_header = TRUE,
                                     London = TRUE,
                                     dotted_line = c(manchester_line_year_1,
                                                     midland_line_year),
                                     inflation = FALSE,
                                     title_text = c("Table LRT0104",
                                                    "Passenger miles on light rail and trams and undergrounds by system: Great Britain - annual from 1983/84"),
                                     units = "Millions",
                                     footer_text = c(foot_system,
                                                     foot_manchester_1,
                                                     foot_blackpool_1,
                                                     foot_midland_2)),
                      LRT0105 = list(decimal_points = 1,
                                     double_header = TRUE,
                                     London = TRUE,
                                     dotted_line = c(manchester_line_year_2,
                                                     midland_line_year),
                                     inflation = FALSE,
                                     title_text = c("Table LRT0105",
                                                    "Vehicle kilometres on light rail and trams and undergrounds by system: Great Britain - annual from 1983/84"),
                                     units = "Millions",
                                     footer_text = c(foot_system,
                                                     foot_manchester_2,
                                                     foot_midland_2)),
                      LRT0106 = list(decimal_points = 1,
                                     double_header = TRUE,
                                     London = TRUE,
                                     dotted_line = c(manchester_line_year_2,
                                                     midland_line_year),
                                     inflation = FALSE,
                                     title_text = c("Table LRT0106",
                                                    "Vehicle miles on light rail and trams and undergrounds by system: Great Britain - annual from 1983/84"),
                                     units = "Millions",
                                     footer_text = c(foot_system,
                                                     foot_manchester_2,
                                                     foot_midland_2)),
                      LRT0107a = list(decimal_points = 1,
                                     double_header = FALSE,
                                     London = TRUE,
                                     dotted_line = midland_line_year,
                                     inflation = FALSE,
                                     title_text = c("Table LRT0107a",
                                                    "Average length of journey in kilometres on light rail and trams by system: England - annual from 2011/12"),
                                     units = "Kilometres",
                                     footer_text = c(foot_system,
                                                     foot_midland_2)),
                      LRT0107b = list(decimal_points = 1,
                                      double_header = FALSE,
                                      London = TRUE,
                                      dotted_line = midland_line_year,
                                      inflation = FALSE,
                                      title_text = c("Table LRT0107b",
                                                     "Average length of journey in miles on light rail and trams by system: England - annual from 2011/12"),
                                      units = "Miles",
                                      footer_text = c(foot_system,
                                                      foot_midland_2)),
                      LRT0108 = list(decimal_points = 0,
                                     double_header = FALSE,
                                     London = FALSE,
                                     dotted_line = midland_line_year,
                                     inflation = FALSE,
                                     title_text = c("Table LRT0108",
                                                    "Average light rail and tram vehicle occupancy by system: England - annual from 2011/12"),
                                     units = "Average vehicle occupancy",
                                     footer_text = c(foot_av_occupancy,
                                                     foot_system,
                                                     foot_manchester_3,
                                                     foot_midland_2)),
                      LRT0109 = list(decimal_points = 1,
                                     double_header = FALSE,
                                     London = TRUE,
                                     dotted_line = c(manchester_line_year_1,
                                                     midland_line_year),
                                     inflation = FALSE,
                                     title_text = c("Table LRT0109",
                                                    "Passenger journeys per head on light rail and trams by system: England - annual from 2004/05"),
                                     units = "Journeys per head",
                                     footer_text = c(foot_journeys_per_head,
                                                     foot_system,
                                                     foot_manchester_1,
                                                     foot_midland_2)),
                      LRT0201 = list(decimal_points = 0,
                                     double_header = TRUE,
                                     London = FALSE,
                                     dotted_line = blackpool_line_year,
                                     inflation = FALSE,
                                     title_text = c("Table LRT0201",
                                                    "Number of stations or stops on light rail and trams and undergrounds by system: Great Britain - annual from 1995/96"),
                                     units = "",
                                     footer_text = c(foot_system,
                                                     foot_blackpool_2)),
                      LRT0202 = list(decimal_points = 0,
                                     double_header = TRUE,
                                     London = TRUE,
                                     inflation = FALSE,
                                     title_text = c("Table LRT0202",
                                                    "Passenger carriages or tram cars on light rail and trams and undergrounds by system: Great Britain - annual from 1983/84"),
                                     units = "",
                                     footer_text = c(foot_system)),
                      LRT0203 = list(decimal_points = 0,
                                     double_header = TRUE,
                                     London = FALSE,
                                     inflation = FALSE,
                                     title_text = c("Table LRT0203",
                                                    "Route kilometres open for passenger traffic on light rail and trams and undergrounds by system: Great Britain - annual from 1995/96"),
                                     units = "Kilometres",
                                     footer_text = c(foot_system,
                                                     foot_glasgow_1)),
                      LRT0204 = list(decimal_points = 0,
                                     double_header = TRUE,
                                     London = FALSE,
                                     inflation = FALSE,
                                     title_text = c("Table LRT0204",
                                                    "Route miles open for passenger traffic on light rail and trams and undergrounds by system: Great Britain - annual from 1995/96"),
                                     units = "Miles",
                                     footer_text = c(foot_system,
                                                     foot_glasgow_1)),
                      LRT0301a = list(decimal_points = 1,
                                      double_header = TRUE,
                                      London = TRUE,
                                      inflation = FALSE,
                                      title_text = c("Table LRT0301a",
                                                     "Passenger revenue at actual prices on light rail and trams and undergrounds by system: Great Britain - annual from 1983/84"),
                                      units = "\U00A3 Millions",
                                      footer_text = c(foot_inflation_1,
                                                      foot_system,
                                                      foot_DLR_1)),
                      LRT0301b = list(decimal_points = 1,
                                      double_header = TRUE,
                                      London = TRUE,
                                      inflation = TRUE,
                                      title_text = c("Table LRT0301b",
                                                     "Passenger revenue at FIN_YEAR prices on light rail and trams and undergrounds by system: Great Britain - annual from 1983/84"),
                                      units = "\U00A3 Millions",
                                      footer_text = c(foot_inflation_2,
                                                      foot_system,
                                                      foot_DLR_1)),
                      LRT0302a = list(decimal_points = 1,
                                      double_header = FALSE,
                                      London = FALSE,
                                      inflation = FALSE,
                                      title_text = c("Table LRT0302a",
                                                     "Concessionary revenue at actual prices on light rail and trams by system: England - annual from 1999/00"),
                                      units = "\U00A3 Millions",
                                      footer_text = c(foot_inflation_1,
                                                      foot_system,
                                                      foot_DLR_2)),
                      LRT0302b = list(decimal_points = 1,
                                      double_header = FALSE,
                                      London = FALSE,
                                      inflation = TRUE,
                                      title_text = c("Table LRT0302b",
                                                     "Concessionary revenue at FIN_YEAR prices on light rail and trams by system: England - annual from 1999/00"),
                                      units = "\U00A3 Millions",
                                      footer_text = c(foot_inflation_2,
                                                      foot_system,
                                                      foot_DLR_2)))

