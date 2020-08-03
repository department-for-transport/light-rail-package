# Constants used in generate_publication_charts

# Theme

theme_dft <- ggplot2::theme(axis.text = ggplot2::element_text(family = "sans", size = 24, colour = "black"),
                            axis.text.x = ggplot2::element_text(margin = ggplot2::margin(20, 0, 0, 0)),
                            axis.text.y = ggplot2::element_text(margin = ggplot2::margin(0, 20, 0, 0)),
                            axis.title.x = ggplot2::element_blank(),
                            axis.title.y = ggplot2::element_blank(),
                            plot.title = ggplot2::element_blank(),
                            plot.subtitle = ggplot2::element_blank(),
                            plot.margin = ggplot2::unit(c(1,1.5,1,1), "cm"),
                            legend.key = ggplot2::element_blank(),
                            legend.position = "bottom",
                            legend.direction = "horizontal",
                            legend.title = ggplot2::element_blank(),
                            legend.text = ggplot2::element_text(size = 9, family = "sans"),
                            axis.ticks = ggplot2::element_blank(),
                            panel.grid = ggplot2::element_blank(),
                            panel.background = ggplot2::element_rect(fil = "#FFFFFF", colour = "#FFFFFF"),
                            axis.line.x = ggplot2::element_line(size = 0.5, colour = "black"))


# Colours

light_blue = "#0099A9"

dark_green = "#006853"

light_green = "#66A498"

orange = "#D25F15"

light_orange = "#E49F73"


# General

line_size = 2.5

circle = 19

circle_size = 10

chart_dpi = 600

chart_height = 8

chart_width = 14

ten_year_comparison <- 11

first_year <- 1

dashed_line <- 2


# Chart 1

lower_chart_1 = 0

step_chart_1 = 50

corner_chart_1 = c(0, 0)


# Chart 2

year_gaps_chart_2 <- 2

step_chart_2 <- 1


# Chart 3

step_chart_3 <- 10

manchester_change_year <- "2011/12"

year_gaps_chart_3 <- 2

dashed_line_size <- 1

chart_3_height <- 8

chart_3_width <- 18


# Chart 4

step_chart_4 <- 20

year_gaps_chart_4 <- 2
