#' Filters survey tibble to give last year entries only
#'
#' This function reduces the survey form tibble down to only the rows where year = "last_year",
#' and drops the columns which don't have a "last_year" field in the form (changes_to_fleet,
#' basis, source_details, description, comments).
#'
#' @param survey_tibble is a tibble containing the data from the survey forms.
#' The tibble structure should match the output of the \code{\link{read_lrt_folder}} function.
#'
#' @return Filtered tibble with "last year" entries only.
#'
#' @examples
#' last_year_only(read_lrt_folder("G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/5. RAP Data/Received Survey forms"))
#'


last_year_only <- function(survey_tibble){

  survey_tibble %>%

    dplyr::filter(year == "last_year") %>%

    dplyr::select(name:variable_costs)

}

#' Checks if rows are identical
#'
#' This function checks if rows from the survey sent forms and the survey response forms are identical.
#' If they are, it returns an empty tibble.
#' If they are not, it returns a tibble with the name of the tram network, the field in which the values
#' are different, and the two different values.
#'
#' @param sent_row is a row from the survey sent forms tibble.
#'
#' @param response_row is a row from the survey response forms tibble.
#'
#' @return If the inputs are identical, the output will be an empty tibble.
#' If the inputs are not identical, the output will be a tibble containing the name of the tram network,
#' the field in which the values are different, and the two different values.
#'


are_rows_identical <- function(sent_row, response_row){

  all_row_differences <- NULL

  for (i in 3:length(sent_row)){

    not_equal <- sent_row[[i]] != response_row[[i]]

    one_is_na <- (is.na(sent_row[[i]]) & !is.na(response_row[[i]])) | (!is.na(sent_row[[i]]) & is.na(response_row[[i]]))

    both_na <- (is.na(sent_row[[i]]) & is.na(response_row[[i]]))

    if ((not_equal | one_is_na) & !both_na){

      row_difference <- tibble::tibble(
        name     = sent_row[["name"]],
        field    = colnames(sent_row)[[i]],
        sent     = sent_row[[i]],
        response = response_row[[i]]
      )

    } else {

      row_difference <- NULL

      }

    all_row_differences <- dplyr::bind_rows(all_row_differences, row_difference)

  }

  return(all_row_differences)

}



#' Check whether "last year" data has been changed by operators
#'
#' This function compares the data from the surveys sent and the survey responses to check
#' whether the "last year" data that was sent in the forms has been changed by the operators.
#'
#' @param survey_sent is a tibble containing the data from the survey forms sent out to operators.
#' The tibble structure should match the output of the \code{\link{read_lrt_folder}} function.
#'
#' @param survey_response is a tibble containing the data from the survey forms returned by operators.
#' The tibble structure should match the output of the \code{\link{read_lrt_folder}} function.
#'
#' @return A tibble with the details of any "last year" values which were changed. If none were changed, returns
#' NULL.
#'
#' @examples
#' last_year_check(read_lrt_folder("G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/5. RAP Data/Survey forms issued"),
#'                 read_lrt_folder("G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/5. RAP Data/Received Survey forms"))
#'

last_year_check <- function(survey_sent, survey_response){

  ly_survey_sent <- tramlr::last_year_only(survey_sent)

  ly_survey_response <- tramlr::last_year_only(survey_response)

  form_differences <- NULL

  for (i in 1:dplyr::count(ly_survey_sent)[[1]]){

    for (j in 1:dplyr::count(ly_survey_response)[[1]]){

      if (ly_survey_sent[i, "name"][[1]] == ly_survey_response[j, "name"][[1]]){

        row_differences <- tramlr::are_rows_identical(ly_survey_sent[i,], ly_survey_response[j,])

      } else{

        row_differences <- NULL

      }

      form_differences <- dplyr::bind_rows(form_differences, row_differences)

    }

  }

  return(form_differences)

}


