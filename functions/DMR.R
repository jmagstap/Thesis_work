#' Load Maine DMR trawl data
#' This function reads a CSV file containing the Maine Department of Marine Resources (DMR)
#' data using `readr::read_csv`. The function returns the data in a tibble format.
#'@param filename A character string specifying the path to the CSV file to be read. 
#'   By default, it points to the "Updated MeDMR data 10_10.csv" file in the "Maine_DMR" directory.
#'   If a different file is needed, the `filename` parameter can be modified.
#' @return An sf table containing the data read from the specified CSV file.

read_DMR = function(filename = file.path("/Users/lizamaguire/Desktop/Thesis_data/Trawl_survey_data/NMFS_DMR_combined_3_14_25.xlsx"), 
      cleanup = TRUE) {
      sdm_data = read_xlsx(filename, sheet = 1)
      if(cleanup) {
        sdm_data = cleanup_DMR(sdm_data)
      } else{
        sdm_data = st_as_sf(sdm_data |>
                              dplyr::filter(!is.na(beg_lon)) , 
                            coords = c("beg_lon", "beg_lat"), 
                            crs = 4326) 
      }
      return(sdm_data)
}


#' Cleanup Maine DMR Data
#'
#' This function processes the Maine Department of Marine Resources (DMR) data by performing
#' several cleaning and transformation steps, including extracting the month, renaming columns,
#' creating a presence-absence column, and converting the data to a spatial format (sf object).
#' @param x A data frame or tibble containing the raw Maine DMR data. The default is to read
#'   the data using the `read_DMR()` function, but a custom data frame can be provided if needed.
#' @return An sf object containing the cleaned Maine DMR data, with columns for latitude, 
#'   longitude, year, month, date, season, number of individuals caught, species name, and 
#'   presence-absence data. The data will be in the WGS 84 coordinate reference system (EPSG:4326).
cleanup_DMR = function(x = read_DMR()){
  x = x |>
    dplyr::mutate(
      pa = 1, #ifelse(EXPCATCHNUM > 0, 1, 0), 
      unique_id = ifelse(Survey == "NMFS", paste0("f", unique_id), paste0("s", unique_id))
    ) |>
    dplyr::select(unique_id, beg_lat, beg_lon, year, month, season, number_caught = EXPCATCHNUM, common_name = SCIENTIFIC_NAME, pa, surf_temp, surf_salin, bot_temp, bot_salin) |>
    dplyr::filter(!is.na(beg_lon))
  
  x <- x %>% 
    sf::st_as_sf(
      coords = c("beg_lon", "beg_lat"),
      crs = st_crs(4326))
  return(x)
}

bb_DMR <- function(x = read_DMR(), bb = get_poly()) {
  if (!is.null(bb)) {
    x <- sf::st_crop(x, bb)
  }
  return(x)
}

#' Create Presence-Absence for a Specific Species
#'
#' This function generates a presence-absence column for a specific species in the cleaned
#' Maine DMR data. If the species is present and the number caught is greater than 0, the
#' presence-absence value is set to 1; otherwise, it is set to 0.
#' @param s A data frame or `sf` object containing the cleaned Maine DMR data. This can be 
#'   the output of the `cleanup_DMR()` function, or any other data frame containing similar 
#'   columns. The default is to use the cleaned DMR data from `cleanup_DMR()`.
#' @param species A character vector containing the species name(s) for which to create the
#'   presence-absence column. Default is `"Haddock"`. This can be adjusted to any other species
#'   of interest or a list of species
#' @return A data frame (or `sf` object if the input is an `sf` object) with an additional column 
#'   `pa` that indicates presence (1) or absence (0) of the specified species based on the 
#'   number of individuals caught.

pa_species_legacy = function(s = cleanup_DMR(), species = "Haddock") {
  s <- s %>%
    dplyr::group_by(unique_id) %>%
    dplyr::group_map(
      function(tbl, key) {
        z = dplyr::filter(tbl, common_name %in% species)
        z |> 
          dplyr::slice(1) |>
          dplyr::mutate(common_name = paste(species, collapse = ","), count = sum(pa), unique_id = key$unique_id) |>
          dplyr::select(unique_id, common_name, count)
        
      }
    ) |> 
    dplyr::bind_rows()
  
  return(s)
}

pa_species2 = function(s = read_DMR(), species = "Haddock") {
  # Process each unique_id group
  s <- s %>%
    dplyr::group_by(unique_id) %>%
    dplyr::group_map(
      function(tbl, key) {
        # Get all species in the current group (unique_id)
        all_species <- unique(tbl$common_name)
        z = select(tbl, year, month)
        # If the species of interest is present, set count to 1, otherwise set count to 0
        if (species %in% all_species) {
          z <- mutate(
            z,
            common_name = species,
            count = 1,  # Set count to 1 if species is found
            unique_id = key$unique_id,
            geometry = tbl$geometry[1], # Keep the geometry from the first row in the group
            bot_temp = tbl$bot_temp[1],
            bot_salin = tbl$bot_salin[1],
            surf_temp = tbl$surf_temp[1],
            surf_salin = tbl$surf_salin[1]
          )
        } else {
          # If the species is not present, set count to 0
          z <- tibble(
            common_name = species,
            count = 0,  # Set count to 0 if species is not found
            unique_id = key$unique_id,
            geometry = tbl$geometry[1], # Keep the geometry from the first row in the group
            bot_temp = tbl$bot_temp[1],
            bot_salin = tbl$bot_salin[1],
            surf_temp = tbl$surf_temp[1],
            surf_salin = tbl$surf_salin[1]
          )
        }
        
        # Select relevant columns to return
        z <- z %>%
          dplyr::select(unique_id, common_name, count, geometry, bot_temp, bot_salin, surf_temp, surf_salin)
        
        return(z)  # Return the processed data
      }
    ) %>%
    dplyr::bind_rows()  # Combine all results into a single data frame
  
  return(s)  # Return the processed spatial data frame
}

pa_species3 = function(s = read_DMR(), species = "Haddock") {
  r = s |> group_by(unique_id) |>
    group_map(
      function(tbl, key){
        z = filter(tbl, common_name == species)
        if(nrow(z) == 0) {
          z = tbl |> dplyr::slice(1) |> dplyr::mutate(pa = 0)
        } else {
          z = z |> dplyr::slice(1)
        }
      }, .keep = TRUE) |>
    bind_rows()
  return(r)
}

strip_tif = function(x = list("filename.tif" = 7, "filename" = "a")){
  names(x) = gsub(".tif", "", names(x), fixed = TRUE)
  return(x)
}
