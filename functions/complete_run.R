#source("Setup.R")

#' This function creates a list of species names used to create and load sdm data files
#' @return A list of species of interest
species_list = function(){
  species = c("Haddock", "Flounder Winter", "Cod Atlantic", "Hake Silver (Whiting)", "Lobster American", "Pollock", "Plaice American (Dab)")
  return(species)
}

#' This function is used to build the frame of a table to input all model information and all parameters
#' @param model_type is the model object
#' @param decade is the decade of interest 
#' @param scenario is the ssp scenario of interest
#' @param species is the species of interest
#' @return a table with all combinations of specified parameters
build_initial_table = function(model_type = c("GAM", "lrg"), 
                               decade = c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090), 
                               scenario = c("ssp245", "ssp370", "ssp585"),
                               species = species_list()){
  tidyr::expand_grid(model_type, decade, scenario, species) |> 
    dplyr::arrange(model_type, scenario, species, decade)
}


#' This function allows us to see which types of models we need for each species
#' @param x the last product returned from build_initial_table function
#' @return A table indicating which model types are needed for each species 
build_model_table = function(x = build_initial_table()){
  dplyr::count(x, model_type, species) |> dplyr::select(-n)
}



#' this function runs the gam model with specified species data
#' @param data specified species data 
#' @return gam model object for species of interest
model_gam = function(data){
  gam_sdm_model <- mgcv::gam(
    pa ~ s(bot_temp, k = 4) + s(bot_salin, k = 4) + s(topographic_position_index, k = 4) + s(aspect, k = 4) + s(bot_ph, k = 4),
    family = binomial(link = "logit"),
    data = data)
  return(gam_sdm_model)
}

#' This function runs the logistic regression model with specified species data
#' @param data specified species data 
#' @return logistic regression model object for species of interest
model_lrg = function(data){
  lrg_model <- glm(pa ~ bot_temp + bot_salin + bot_ph + aspect + topographic_position_index, family = binomial(link = "logit"), data = data)
  return(lrg_model)
}


#' This function loads all species data from "4.28.25.species.data" folder
load_species = function(species = species_list(), path = "~/Desktop/Thesis_data/4.28.25.species.data", drop_geometry = FALSE){
  #"~/Desktop/Thesis_data/4.28.25.species.data/flounder winter_sdm_data.gpkg"
  load_species_1 = function(species = "Haddock"){
    f_name = sprintf("%s_sdm_data.gpkg", tolower(species[1]))
    x = sf::read_sf(file.path(path, f_name))
    if(drop_geometry) x = sf::st_drop_geometry(x)
    return(x)
  }
  
  sapply(species, load_species_1, simplify = FALSE)
}
  

populate_model_table = function(x = build_model_table(), data = load_species()){
  x |> dplyr::rowwise()|> 
    dplyr::group_map(
      function(tbl, key){
        species_data = data[[tbl$species]]
        y = switch(tbl$model_type, 
               "GAM" = model_gam(species_data), 
               "lrg" = model_lrg(species_data),
               "foo" = model_foo(species_data),
               stop("model_type not known:", tbl$model_type))
        tbl$model = list(y)
        return(tbl)
      }
    ) |> 
    dplyr::bind_rows()
}

  