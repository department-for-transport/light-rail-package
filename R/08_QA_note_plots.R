#' Plots scatter graph
#'
#' This function takes a light rail and tram feature table, a HEX colour code, and two financial
#' years and plots a scatter graph where x = last_year and y = this_year.
#'
#' @param lrt_tbl is a tibble containing the \code{last_year}, \code{this_year}, \code{diff}
#' and \code{percent_change} figures for a given feature (e.g. route_km_tbl) by tram system.
#' The tibble structure should match the tibbles stored in the list output of the
#' \code{\link{lrt_make_all_tables}} function.
#'
#' @param plot_colour is the HEX colour code for the plot, default is orange.
#'
#' @param previous_fin_year is a string containing the financial year before the one to which the
#' survey data refers. For example: "2018/19".
#'
#' @param publication_fin_year is a string containing the financial year to which the survey data
#' refers. For example: "2019/20".
#'
#' @return Dynamic scatter plot of this year vs last year.
#'
#' @examples
#' lrt_scatter_plot(params$no_of_stops_tbl, previous_fin_year, publication_fin_year)
#'

lrt_scatter_plot <- function(lrt_tbl, previous_fin_year, publication_fin_year, lrt_colour = "#d25f15"){

  plotly::ggplotly(ggplot2::ggplot(lrt_tbl, ggplot2::aes(x = last_year, y = this_year)) +
                     ggplot2::geom_point(colour = lrt_colour,
                                ggplot2::aes(text = paste(name,
                                                 "<br>",
                                                 previous_fin_year, ": ", format(lrt_tbl$last_year, big.mark = ",", trim = TRUE),
                                                 "<br>",
                                                 publication_fin_year, ": ", format(lrt_tbl$this_year, big.mark = ",", trim = TRUE),
                                                 "<br>",
                                                 "Change: ", percent_change, "%",
                                                 sep = ""))) +
                     ggplot2::geom_abline(slope = 1, intercept = 0,
                                 linetype = "dashed", colour = "grey") +
                     ggplot2::xlab(previous_fin_year) +
                     ggplot2::ylab(publication_fin_year),
                   tooltip = "text") %>%
    plotly::layout(xaxis = list(title = previous_fin_year),
           yaxis = list(title = publication_fin_year),
           paper_bgcolor = "transparent",
           plot_bgcolor = "#ededed")

}


#' Plots bar chart
#'
#' This function takes a light rail and tram feature table, a HEX colour code, and two
#' financial years and plots a bar chart of percentage change for each network.
#'
#' @param lrt_tbl is a tibble containing the \code{last_year}, \code{this_year}, \code{diff}
#' and \code{percent_change} figures for a given feature (e.g. route_km_tbl) by tram system.
#' The tibble structure should match the tibbles stored in the list output of the
#' \code{\link{lrt_make_all_tables}} function.
#'
#' @param plot_colour is the HEX colour code for the plot, default is orange.
#'
#' @param previous_fin_year is a string containing the financial year before the one to which the
#' survey data refers. For example: "2018/19".
#'
#' @param publication_fin_year is a string containing the financial year to which the survey data
#' refers. For example: "2019/20".
#'
#' @return Dynamic bar chart of percentage change by network.
#'
#' @examples
#' lrt_bar_chart(params$no_of_stops_tbl, previous_fin_year, publication_fin_year)
#'

lrt_bar_chart <- function(lrt_tbl, previous_fin_year, publication_fin_year, lrt_colour = "#d25f15"){

  lrt_tbl$this_year <- format(lrt_tbl$this_year, big.mark = ",", trim = TRUE)

  lrt_tbl$last_year <- format(lrt_tbl$last_year, big.mark = ",", trim = TRUE)

  plotly::ggplotly(ggplot2::ggplot(lrt_tbl, ggplot2::aes(x = name, y = percent_change)) +
                     ggplot2::geom_bar(stat = "identity",
                              fill = lrt_colour,
                              ggplot2::aes(text = paste(name,
                                               "<br>",
                                               previous_fin_year, ": ", last_year,
                                               "<br>",
                                               publication_fin_year, ": ", this_year,
                                               "<br>",
                                               "Change: ", percent_change, "%",
                                               sep = ""))) +
                     ggplot2::coord_flip(),
                   tooltip = "text") %>%
    plotly::layout(xaxis = list(title = paste("% change between", previous_fin_year, "and", publication_fin_year, sep = " "),
                        zeroline = TRUE,
                        zerolinecolor = "#cfcfcf",
                        zerolinewidth = 2),
           yaxis = list(title = "",
                        autorange = "reversed"),
           paper_bgcolor = "transparent",
           plot_bgcolor = "#ededed")

}



#' Displays table in rmarkdown
#'
#' This function takes a light rail and tram feature table, a caption, and two financial years
#' and displays a table in rmarkdown.
#'
#' @param lrt_tbl is a tibble containing the \code{last_year}, \code{this_year}, \code{diff}
#' and \code{percent_change} figures for a given feature (e.g. route_km_tbl) by tram system.
#' The tibble structure should match the tibbles stored in the list output of the
#' \code{\link{lrt_make_all_tables}} function.
#'
#' @param lrt_caption is a string that appears above the table. The default is an empty string.
#'
#' @param previous_fin_year is a string containing the financial year before the one to which the
#' survey data refers. For example: "2018/19".
#'
#' @param publication_fin_year is a string containing the financial year to which the survey data
#' refers. For example: "2019/20".
#'
#' @return A table in rmarkdown.
#'
#' @examples
#' lrt_rmd_table(params$no_of_stops_tbl, previous_fin_year, publication_fin_year, lrt_caption = "Number of tram stops")
#'

lrt_rmd_table <- function(lrt_tbl, previous_fin_year, publication_fin_year, lrt_caption = ""){

  lrt_tbl$this_year <- format(lrt_tbl$this_year, big.mark = ",", trim = TRUE)

  lrt_tbl$last_year <- format(lrt_tbl$last_year, big.mark = ",", trim = TRUE)

  lrt_tbl$diff <- format(lrt_tbl$diff, big.mark = ",", trim = TRUE)

  knitr::kable(lrt_tbl,
               caption = lrt_caption,
               align = "lrrrr",
               col.names = c("Network",
                             previous_fin_year,
                             publication_fin_year,
                             "Difference",
                             "Percentage change"))

}
