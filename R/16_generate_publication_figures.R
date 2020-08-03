#' Produces an HTML document with all the figures needed for the publication
#'
#' This function reads the data in the minimal tidy dataset and produces an HTML document with
#' RMarkdown which contains all the figures needed for the light rail and tram publication.
#'
#' @param minimal_tidy_dataset_path is a string containing the file path of the Minimal Tidy Dataset
#' excel file. For Windows paths, each backslash must be changed to either a forward slash "/"
#' or two backslashes "\\\\".
#'
#' @param save_publication_figures_path is a string containing the file path of the folder in which
#' you would like to save the HTML file. For Windows paths, each backslash
#' must be changed to either a forward slash "/" or two backslashes "\\\\".
#'
#' @param publication_fin_year is a string containing the financial year to which the survey data
#' refers. For example: "2019/20".
#'
#' @examples
#' generate_publication_figures("G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/7. Minimal Tidy Dataset/2020_minimal_tidy_dataset.xlsx",
#'                              "G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0002 Publication/20-05 Light Rail Statistics",
#'                              "2019/20")

generate_publication_figures <- function(minimal_tidy_dataset_path,
                                         save_publication_figures_path,
                                         publication_fin_year,
                                         figures_author = NULL){

  # Check inputs

  tramlr::check_path(minimal_tidy_dataset_path)

  tramlr::check_path(save_publication_figures_path)

  tramlr::check_financial_year(publication_fin_year)

  if (!is.null(figures_author)){

    tramlr::check_string(figures_author)

  }


  # Get data into correct format

  minimal_tidy_dataset <- tramlr::read_min_tidy_dataset(minimal_tidy_dataset_path)



  # Set title of HTML doc, file name and R Markdown path

  figures_title <- paste(publication_fin_year, "Light Rail and Tram Publication Figures", sep = " ")

  publication_fin_year_file_name <- gsub("/", "-", publication_fin_year)

  output_file_name <- paste(publication_fin_year_file_name, " Light Rail and Tram Publication Figures", ".html", sep = "")

  rmarkdown_path <- system.file("rmd", "publication_figures.Rmd", package = "tramlr", mustWork = TRUE)



  # Set up params

  publication_figures_params <- list("minimal_tidy_dataset" = minimal_tidy_dataset,
                                     "publication_fin_year" = publication_fin_year,
                                     "figures_title" = figures_title,
                                     "figures_author" = figures_author)




  # Render HTML document

  rmarkdown::render(rmarkdown_path,
                    output_format = "html_document",
                    output_file = output_file_name,
                    output_dir = save_publication_figures_path,
                    params = publication_figures_params)

}
