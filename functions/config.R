#' Read Configuration File
#'
#' This function reads a YAML configuration file and sets options based on the content. 
#' It also allows for overriding the `sdmpredictors_datadir` option via a function parameter.
#'
#' @param filename A character string specifying the path to the YAML configuration file. 
#' Defaults to `file.path(THESIS_DATA, "config", "v0.yaml")`.
#' @param sdmpredictors_datadir A character string specifying the path to the `sdmpredictors` data directory. 
#' If not provided, it is read from the YAML file.
#'
#' @return A list containing the parsed YAML configuration.

read_config = function(filename = file.path(THESIS_DATA, "config", "v0.yaml")){
  x = read_yaml(filename)
  options(sdmpredictors_datadir = x$sdmpredictors_datadir)
  return(x)
}

#' Generate Bounding Box from Configuration
#'
#' This function creates a Gulf of Maine specific bounding box (BB) as a Simple Feature (sf) object 
#' from a configuration file containing the bounding box coordinates and CRS.
#' 
#' @param cfg A configuration object, typically read from a YAML file using `read_config()`. 
#' @return A Simple Feature (sf) object representing the bounding box.
#' @examples
#' # Assuming that read_config() has been executed and the config object is available:
#' bbox <- get_bb(cfg)
#'
#' # Directly using the configuration from read_config()
#' bbox <- get_bb()
get_bb = function(cfg = read_config()){
  sf::st_bbox(c(xmin = cfg$lonmin, xmax = cfg$lonmax, ymin = cfg$latmin, ymax = cfg$latmax),
              crs = cfg$crs) |> 
    sf::st_as_sfc()
}

get_poly = function(cfg = read_config()){
st_as_sfc(cfg$large_polygon, crs=4326)
}
