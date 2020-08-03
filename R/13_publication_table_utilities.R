#' Adds a dotted line to the publication table in specified position
#'
#' This function adds dotted lines to the publication excel table. The
#' positions of the dotted lines are specified by the dotted_line entry in
#' the table_content list.
#'
#' @param minimal_tidy_df is a dataframe which matches the format of the tibbles contained in the
#' minimal tidy dataset list.
#'
#' @param all_title_text is the vector of title text in the table.
#'
#' @param LRT_tab is the openxlsx object containing the data for the table.
#'
#' @param table_key is the reference for the table in the table_content list.
#'

add_dotted_line <- function(minimal_tidy_df,
                            all_title_text,
                            LRT_tab,
                            table_key){

  # The column name and row date are stored in pairs in the dotted_line vector
  # So if the length of dotted_line is 4, there are two pairs, each corresponding
  # to a dotted line

  for (i in 1:(length(table_content[[table_key]]$dotted_line) / 2)){

    # Get name index

    j <- (i - 1) * 2 + 1

    # Get date index

    k <- j + 1


    # Find position of cell in dataframe

    # Use date to find row

    cell_row <- grep(table_content[[table_key]]$dotted_line[k], minimal_tidy_df$`Financial year`)

    if (table_content[[table_key]]$double_header){

      cell_row <- cell_row + length(all_title_text) + 2

    } else {

      cell_row <- cell_row + length(all_title_text) + 1

    }

    # Use column name to find column

    cell_column <- grep(table_content[[table_key]]$dotted_line[j], colnames(minimal_tidy_df))


    # Add dotted line

    openxlsx::addStyle(LRT_tab$wb, 1, dotted_bottom_border, rows = cell_row, cols = cell_column, stack = TRUE)

  }

}


#' Sets the heights of cells with long footers
#'
#' This function sets the heights of the cells containing the footers which are longer than the
#' width of the table.
#'
#' @param table_key is the reference for the table in the table_content list.
#'
#' @param all_title_text is the vector of title text in the table.
#'
#' @param headers_all is the vecotr containing the table headers.
#'
#' @param minimal_tidy_df is a dataframe which matches the format of the tibbles contained in the
#' minimal tidy dataset list.
#'
#' @param footer_text is the vector containing all of the table footers.
#'
#' @param LRT_tab is the openxlsx object containing the data for the table.
#'


set_footer_row_heights <- function(table_key,
                                   all_title_text,
                                   headers_all,
                                   minimal_tidy_df,
                                   footer_text,
                                   LRT_tab){

  for (i in 1:length(long_footers)){

    if (any(grepl(eval(parse(text = names(long_footers)[[i]])), table_content[[table_key]]$footer_text, fixed = TRUE))){


      # Find the row of the long footer

      long_footer_row <- length(all_title_text) + length(headers_all) + dplyr::count(minimal_tidy_df)[[1]]
      long_footer_row <- long_footer_row + grep(eval(parse(text = names(long_footers)[[i]])), footer_text, fixed = TRUE)


      # Get the row height

      row_height <- normal_row_height * long_footers[[i]]


      # Set the row height

      openxlsx::setRowHeights(LRT_tab$wb,
                              sheet = 1,
                              rows = c(long_footer_row),
                              heights = rep(row_height, 1))

    }

  }

}

