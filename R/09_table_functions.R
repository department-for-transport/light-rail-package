##Table functions; individual functions to process table data
##Make summary columns for London, England, GB, etc
GB_summary <- function(data){
  data %>%
    ##Add the date
    dplyr::mutate(`Financial year` = publication_fin_year,
                  London = sum(`Docklands Light Railway`, `London Tramlink`,
                               na.rm = TRUE),
                  `England outside of London` = sum(`Nottingham Express Transit`,
                                                    `Midland Metro`,
                                                    `Sheffield Supertram`,
                                                    `Tyne And Wear Metro`,
                                                    `Manchester Metrolink`,
                                                    `Blackpool Tramway`),

                  England = sum(London, `England outside of London`),
                  GB = sum(England, `Edinburgh Trams`))
}

##Make summary columns for London and England only, and remove Scottish columns
england_summary <- function(data){
  data %>%
    ##Add the date
    dplyr::mutate(`Financial year` = publication_fin_year,
                  `England outside of London` = sum(`Nottingham Express Transit`,
                                                    `Midland Metro`,
                                                    `Sheffield Supertram`,
                                                    `Tyne And Wear Metro`,
                                                    `Manchester Metrolink`,
                                                    `Blackpool Tramway`),

                  England = sum(`Docklands Light Railway`, `London Tramlink`,
                                `England outside of London`)) %>%
    ##Remove Scottish trams
    dplyr::select(-c(`Edinburgh Trams`, `Glasgow Underground`))
}

##Individual table functions-------------------------------------

# LRT0101 Passenger Journeys ===================================================
lrt0101 <- function(new = new_data, old = min_tidy_dataset){

  #Move new data into wide format
 new <- new %>%
    dplyr::select(name, total_boardings) %>%
    ##convert total boardings into millions
    dplyr::mutate(total_boardings = total_boardings/1000000) %>%
    tidyr::pivot_wider(names_from = name, values_from = total_boardings) %>%
    GB_summary()

 ##Find our data in the list
  dplyr::bind_rows(old[[grep("0101", names(old))]],
                   new)
}

# LRT0102 Concessionary Journeys =============================================
lrt0102 <- function(new = new_data, old = min_tidy_dataset){

  #Move new data into wide format
  new <- new %>%
    dplyr::select(name, cons_boardings) %>%
    ##convert boardings into millions
    dplyr::mutate(cons_boardings = cons_boardings/1000000) %>%
    tidyr::spread(name, cons_boardings) %>%
    england_summary()

  ##Find our data in the list
  dplyr::bind_rows(old[[grep("0102", names(old))]],
                   new)
}

# LRT0103 Passenger Kilometers  ================================================
lrt0103 <- function(new = new_data, old = min_tidy_dataset) {
  #Move new data into wide format
  new <- new %>%
    dplyr::select(name, passenger_km) %>%
    ##convert passenger km into millions
    dplyr::mutate(passenger_km = passenger_km/1000000) %>%
    tidyr::spread(name, passenger_km) %>%
    GB_summary()

  ##Find our data in the list
  dplyr::bind_rows(old[[grep("0103", names(old))]],
                   new)
}

# LRT0104 Passenger Miles  ==================================================
lrt0104 <- function(new = new_data, old = min_tidy_dataset) {

    #Move new data into wide format
  new <- new %>%
    dplyr::select(name, passenger_km) %>%
    ##convert passenger km into millions miles
    dplyr::mutate(passenger_km = measurements::conv_unit(passenger_km/1000000,
                                                         "km", "mi")) %>%
    tidyr::spread(name, passenger_km) %>%
    GB_summary()

  ##Find our data in the list
  dplyr::bind_rows(old[[grep("0104", names(old))]],
                   new)
}


# LRT0105 Kilometers Operated  =================================================
lrt0105 <- function(new = new_data, old = min_tidy_dataset) {

  #Move new data into wide format
  new <- new %>%
    dplyr::select(name, km_operated) %>%
    ##convert passenger km into millions
    dplyr::mutate(km_operated = km_operated/1000000) %>%
    tidyr::spread(name, km_operated) %>%
    # Glasgow underground is divided by 3 because they count per carriage
    #but there are 3 carriages per tram
    dplyr::mutate(`Glasgow Underground` = `Glasgow Underground`/3) %>%
    GB_summary()

  ##Find our data in the list
  dplyr::bind_rows(old[[grep("0105", names(old))]],
                   new)
}

# LRT0106 Miles Operated ========================================================================================================================================================================

lrt0106 <- function(new = new_data, old = min_tidy_dataset) {

  #Move new data into wide format
  new <- new %>%
    dplyr::select(name, km_operated) %>%
    ##convert passenger km into millions of miles
    dplyr::mutate(km_operated =
                    measurements::conv_unit(km_operated, "km", "mi")/1000000) %>%
    tidyr::spread(name, km_operated) %>%
    # Glasgow underground is divided by 3 because they count per carriage
    #but there are 3 carriages per tram
    dplyr::mutate(`Glasgow Underground` = `Glasgow Underground`/3) %>%
    GB_summary()

  ##Find our data in the list
  dplyr::bind_rows(old[[grep("0106", names(old))]],
                   new)
}

# LRT0107a Average Length of Journey (km) ==================================
lrt0107a <- function(new = new_data, old = min_tidy_dataset) {

  new <- new %>%
    dplyr::select(name, passenger_km, total_boardings) %>%
    ##Pivot data to calculate england, etc breakdowns
    tidyr::pivot_longer(cols = -name, names_to = "variable") %>%
    tidyr::pivot_wider(names_from = name, values_from = value) %>%
    england_summary() %>%
    #Move back into previous structure
    tidyr::pivot_longer(cols = -c(variable, `Financial year`)) %>%
    tidyr::pivot_wider(names_from = variable, values_from = value) %>%
    ##Calculate km per journey
    dplyr::mutate(km_journey = passenger_km/total_boardings) %>%
    ##Remove calculation columns
    dplyr::select(-passenger_km, -total_boardings) %>%
    tidyr::pivot_wider(names_from = name, values_from = km_journey)

  ##Find our data in the list
  dplyr::bind_rows(old[[grep("0107a", names(old))]],
                   new)
}

# LRT0107b Average Length of Journey (miles) ==============================

lrt0107b <- function(new = new_data, old = min_tidy_dataset) {

  new <- new %>%
    dplyr::select(name, passenger_km, total_boardings) %>%
    ##Pivot data to calculate england, etc breakdowns
    tidyr::pivot_longer(cols = -name, names_to = "variable") %>%
    tidyr::pivot_wider(names_from = name, values_from = value) %>%
    england_summary() %>%
    #Move back into previous structure
    tidyr::pivot_longer(cols = -c(variable, `Financial year`)) %>%
    tidyr::pivot_wider(names_from = variable, values_from = value) %>%
    ##Calculate miles per journey
    dplyr::mutate(m_journey = measurements::conv_unit(passenger_km/total_boardings,
                                                      "km", "mi")) %>%
    ##Remove calculation columns
    dplyr::select(-passenger_km, -total_boardings) %>%
    tidyr::pivot_wider(names_from = name, values_from = m_journey)

  ##Find our data in the list
  dplyr::bind_rows(old[[grep("0107b", names(old))]],
                   new)
}

# LRT0108 Average Occupancy  ===================================================
lrt0108 <- function(new = new_data, old = min_tidy_dataset) {

  new <- new %>%
    dplyr::select(name, passenger_km, km_operated) %>%
    ##Pivot data to calculate england, etc breakdowns
    tidyr::pivot_longer(cols = -name, names_to = "variable") %>%
    tidyr::pivot_wider(names_from = name, values_from = value) %>%
    england_summary() %>%
    #Move back into previous structure
    tidyr::pivot_longer(cols = -c(variable, `Financial year`)) %>%
    tidyr::pivot_wider(names_from = variable, values_from = value) %>%
    ##Calculate average occupancy
    dplyr::mutate(occ = passenger_km/km_operated) %>%
    ##Remove calculation columns
    dplyr::select(-passenger_km, -km_operated) %>%
    tidyr::pivot_wider(names_from = name, values_from = occ)

  ##Find our data in the list
  dplyr::bind_rows(old[[grep("0108", names(old))]],
                   new)
}


# LRT0109 Passenger Journeys per head  =========================================
lrt0109 <- function(new = new_data, old = min_tidy_dataset) {

  new <- new %>%
    dplyr::select(name, total_boardings) %>%
    ##Pivot data to calculate england, etc breakdowns
    tidyr::pivot_longer(cols = -name, names_to = "variable") %>%
    tidyr::pivot_wider(names_from = name, values_from = value) %>%
    england_summary() %>%
    #Move back into previous structure
    tidyr::pivot_longer(cols = -c(variable, `Financial year`)) %>%
    tidyr::pivot_wider(names_from = variable, values_from = value) %>%
    ##Join on population data
    dplyr::left_join(population_mye) %>%
    ##Calculate average occupancy
    dplyr::mutate(per_head = total_boardings/pop) %>%
    ##Remove calculation columns
    dplyr::select(-total_boardings, -pop) %>%
    tidyr::pivot_wider(names_from = name, values_from = per_head)

  ##Find our data in the list
  dplyr::bind_rows(old[[grep("0109", names(old))]],
                   new)
}

# LRT0201 Number of Stops ======================================================
lrt0201 <- function(new = new_data, old = min_tidy_dataset) {

  new <- new %>%
    dplyr::select(name, no_of_stops) %>%
    tidyr::pivot_wider(names_from = name, values_from = no_of_stops) %>%
    GB_summary()

  ##Find our data in the list
  dplyr::bind_rows(old[[grep("0201", names(old))]],
                   new)
}


# LRT0202 Number of Tram Cars ==================================================
lrt0202 <- function(new = new_data, old = min_tidy_dataset) {

  new <- new %>%
    dplyr::select(name, no_of_vehicles) %>%
    tidyr::pivot_wider(names_from = name, values_from = no_of_vehicles) %>%
    GB_summary()

  ##Find our data in the list
  dplyr::bind_rows(old[[grep("0202", names(old))]],
                   new)
}


# LRT0203 Route Kilometers =====================================================
lrt0203 <- function(new = new_data, old = min_tidy_dataset) {

  #Move new data into wide format
  new <- new %>%
    dplyr::select(name, route_km) %>%
    tidyr::spread(name, route_km) %>%
    # Glasgow underground is divided by 2 because they count both directions
    dplyr::mutate(`Glasgow Underground` = `Glasgow Underground`/2) %>%
    GB_summary()

  ##Find our data in the list
  dplyr::bind_rows(old[[grep("0203", names(old))]],
                   new)
}

# LRT0204 Route Miles ==========================================================
lrt0204 <- function(new = new_data, old = min_tidy_dataset) {

  #Move new data into wide format
  new <- new %>%
    dplyr::select(name, route_km) %>%
    ##convert km into miles
    dplyr::mutate(route_mi = measurements::conv_unit(route_km,
                                                     "km", "mi")) %>%
    tidyr::spread(name, route_mi) %>%
    # Glasgow underground is divided by 2 because they count both directions
    dplyr::mutate(`Glasgow Underground` = `Glasgow Underground`/2) %>%
    GB_summary()

  ##Find our data in the list
  dplyr::bind_rows(old[[grep("0204", names(old))]],
                   new)
}

# LRT0301a Passenger Revenue at actual prices ==================================
lrt0301a <- function(new = new_data, old = min_tidy_dataset) {

  new <- new %>%
    dplyr::rowwise() %>%
    ##Create total revenue in millions
    dplyr::mutate(rev = sum(passenger_receipts, cons_eld_dis, cons_young,
                            na.rm = TRUE)/1000000) %>%
    ##Only keep new calculated values
    dplyr::select(name, rev) %>%
    tidyr::spread(name, rev) %>%
    GB_summary()

  ##Find our data in the list
  dplyr::bind_rows(old[[grep("0301a", names(old))]],
                   new)
}

# LRT0301b Passenger Revenue at current year prices ============================
lrt0301b <- function() {

  lrt0301a() %>%
    unique() %>%
    ##Gather previously calculated data
    tidyr::pivot_longer(cols = -`Financial year`) %>%
    dplyr::left_join(gdp_deflator,
                     by = c("Financial year" = "fin_year")) %>%
    ##Calculate deflated revenue shenanigans
    dplyr::mutate(value = value * relative_deflator) %>%
    ##Remove deflator value
    dplyr::select(-relative_deflator) %>%
    #Spread values out again
    tidyr::pivot_wider(names_from = name, values_from = value)

}
# LRT0302a Concessionary Revenue at actual prices ==============================
lrt0302a <- function(new = new_data, old = min_tidy_dataset) {

  new <- new %>%
    dplyr::rowwise() %>%
    ##Create concession revenue in millions
    dplyr::mutate(rev = sum(cons_eld_dis, cons_young,
                            na.rm = TRUE)/1000000) %>%
    ##Only keep new calculated values
    dplyr::select(name, rev) %>%
    tidyr::spread(name, rev) %>%
    GB_summary()

  ##Find our data in the list
  dplyr::bind_rows(old[[grep("0302a", names(old))]],
                   new)
}

# LRT0302b Concessionary Revenue at current year prices ========================
lrt0302b <- function() {

  lrt0302a() %>%
    unique() %>%
    ##Gather previously calculated data
    tidyr::pivot_longer(cols = -`Financial year`) %>%
    dplyr::left_join(gdp_deflator,
                     by = c("Financial year" = "fin_year")) %>%
    ##Calculate deflated revenue shenanigans
    dplyr::mutate(value = value * relative_deflator) %>%
    ##Remove deflator value
    dplyr::select(-relative_deflator) %>%
    #Spread values out again
    tidyr::pivot_wider()

}

# population tab ===============================================================
population_tab <- function(mye = population_mye) {

  mye <- mye %>%
    tidyr::pivot_wider(names_from = name, values_from = pop)
}
