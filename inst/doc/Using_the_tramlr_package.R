## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE---------------------------------------------------------
#  devtools::load_all()

## ---- eval=FALSE---------------------------------------------------------
#  ?generate_survey_forms

## ---- eval=FALSE---------------------------------------------------------
#  financial_year <- "2020/21"
#  survey_deadline <- "4th May 2020"
#  last_surveys_folder_path <- "G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/3. Survey/Validated survey forms"
#  save_surveys_path <- "G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2021/2. Survey set up"
#  
#  generate_survey_forms(financial_year,
#                        survey_deadline,
#                        last_surveys_folder_path,
#                        save_surveys_path)

## ---- eval=FALSE---------------------------------------------------------
#  ?generate_lrt_QA_note

## ---- eval=FALSE---------------------------------------------------------
#  survey_sent_path <- "G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/2. Survey set up/Survey forms issued"
#  survey_response_path <- "G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/3. Survey/Received Survey forms"
#  save_QA_note_path <- "G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/6. Validation"
#  publication_fin_year <- "2019/20"
#  email_response_path <- "G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/3. Survey/Survey response emails.xlsx"
#  
#  generate_lrt_QA_note(survey_sent_path,
#                       survey_response_path,
#                       save_QA_note_path,
#                       publication_fin_year,
#                       email_response_path)

## ---- eval=FALSE---------------------------------------------------------
#  ?update_minimal_tidy_dataset

## ---- eval=FALSE---------------------------------------------------------
#  min_tidy_dataset_path <- "G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/7. Minimal Tidy Dataset/2019_minimal_tidy_dataset.xlsx"
#  survey_folder_path <- "G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/3. Survey/Validated survey forms"
#  gdp_deflator_path <- "G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/4. Data Entry and Table Compilation/ONS/GDP_Deflators_Qtrly_National_Accounts_March_2020.xlsx"
#  population_mye_path <- "G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/4. Data Entry and Table Compilation/ONS/ukmidyearestimates20182019ladcodes.xls"
#  publication_fin_year <- "2019/20"
#  save_min_tidy_dataset <- "G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/7. Minimal Tidy Dataset"
#  
#  update_minimal_tidy_dataset(min_tidy_dataset_path,
#                              survey_folder_path,
#                              gdp_deflator_path,
#                              population_mye_path,
#                              publication_fin_year,
#                              save_min_tidy_dataset)

## ---- eval=FALSE---------------------------------------------------------
#  ?generate_publication_tables

## ---- eval=FALSE---------------------------------------------------------
#  minimal_tidy_dataset_path <- "G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/7. Minimal Tidy Dataset/2020_minimal_tidy_dataset.xlsx"
#  save_publication_tables_path <- "G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0002 Publication/20-05 Light Rail Statistics/Tables"
#  publication_fin_year <- "2019/20"
#  publication_date <- "25th June 2020"
#  
#  generate_publication_tables(minimal_tidy_dataset_path,
#                              save_publication_tables_path,
#                              publication_fin_year,
#                              publication_date)

## ---- eval=FALSE---------------------------------------------------------
#  dlr_line_year <- c("Docklands Light Railway", "2019/20")

## ---- eval=FALSE---------------------------------------------------------
#  foot_example_1 <- "This is an example footer"

## ---- eval=FALSE---------------------------------------------------------
#  table_content <- list(LRT0101 = list(decimal_points = 1,
#                                       double_header = TRUE,
#                                       London = TRUE,
#                                       dotted_line = c(manchester_line_year_1,
#                                                       midland_line_year,
#                                                       dlr_line_year), # New dotted line added
#                                       inflation = FALSE,
#                                       title_text = c("Table LRT0101",
#                                                      "Passenger journeys on light rail and trams and undergrounds by system: Great Britain - annual from 1983/84"),
#                                       units = "Millions",
#                                       footer_text = c(foot_system,
#                                                       foot_manchester_1,
#                                                       foot_example_1, # New footer added
#                                                       foot_midland_1,
#                                                       foot_midland_2)), # List continues...

## ---- eval=FALSE---------------------------------------------------------
#  # Long footers
#  
#  long_footers <- list(foot_manchester_2 = 3,
#                       foot_blackpool_2 = 2,
#                       foot_example_1 = 4)

## ---- eval=FALSE---------------------------------------------------------
#  ?generate_publication_charts

## ---- eval=FALSE---------------------------------------------------------
#  minimal_tidy_dataset_path <- "G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/7. Minimal Tidy Dataset/2020_minimal_tidy_dataset.xlsx"
#  save_publication_charts_path <- "G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0002 Publication/20-05 Light Rail Statistics"
#  publication_fin_year <- "2019/20"
#  
#  generate_publication_charts(minimal_tidy_dataset_path,
#                              save_publication_charts_path,
#                              publication_fin_year)

## ---- eval=FALSE---------------------------------------------------------
#  ?generate_publication_figures

## ---- eval=FALSE---------------------------------------------------------
#  minimal_tidy_dataset_path <- "G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/7. Minimal Tidy Dataset/2020_minimal_tidy_dataset.xlsx"
#  save_publication_figures_path <- "G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0002 Publication/20-05 Light Rail Statistics"
#  publication_fin_year <- "2019/20"
#  
#  generate_publication_figures(minimal_tidy_dataset_path,
#                               save_publication_figures_path,
#                               publication_fin_year)

