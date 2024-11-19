read_DMR = function(filename = file.path(THESIS_DATA, "Maine_DMR", "Updated MeDMR data 10_10.csv")) {
  sdm_data = readr::read_csv(filename, show_col_types = FALSE)
  return(sdm_data)
}

cleanup_DMR = function(x = read_DMR()){
  
  # Extract the month and create a new column
  x = dplyr::mutate(x, month = lubridate::month(Date), .after = Date) |>
    dplyr::relocate(Year, .after = Date)
  
  x = dplyr::rename(x, 
                    temp_long = Longitude,
                    Longitude = Latitude) |>
                    dplyr::rename(Latitude = temp_long) |>
    dplyr::mutate(
      pa = ifelse(Number_Caught > 0, 1, 0)
    ) |>
    dplyr::select(Latitude, Longitude, Year, month, Date, Season, Number_Caught, Common_Name, pa)
  
  x <- x %>% 
    sf::st_as_sf(
      coords = c("Longitude", "Latitude"),
      crs = st_crs(4326))
  return(x)
}

pa_species = function(s = cleanup_DMR(), species = "Haddock") {
    s <- s %>%
      mutate(
        Number_Caught = as.numeric(Number_Caught),
        pa = ifelse(Common_Name %in% species & Number_Caught > 0, 1, 0)
      )
    return(s)
}

