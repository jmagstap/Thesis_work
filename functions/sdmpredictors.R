#' Get Pretty Names for Environmental Variables
#' 
#' This function returns a named character vector of "pretty" names for various 
#' environmental variables that have more descriptive names for easier interpretation.
#'
#' @return A named character vector where the names are the original variable codes 
#' and the values are the more readable, descriptive names.
pretty_names = function(){
  c("BO21_tempmean_bdmax_lonlat" = "bot_temp_mean",
    "BO21_tempmean_ss_lonlat" = "surf_temp_mean", 
    "BO22_chlomean_ss_lonlat" = "surf_chlo_mean", 
    "BO22_salinitymean_bdmax_lonlat" = "bot_sal_mean",
    "BO22_salinitymean_ss_lonlat" = "surf_sal_mean", 
    "Present"= "pres")
}

#' List Available SDM Predictors
#'
#' This function lists available SDM predictors based on the directory specified in 
#' the `sdmpredictors_datadir` option. It checks if the directory exists and returns 
#' a list of available predictors.
#'@param path A character string specifying the directory containing SDM predictors. 
#' If not provided, the function uses the option set in `sdmpredictors_datadir`. 
#' The default is the `sdmpredictors_datadir` option.
#' @return A list of available SDM predictors. If the directory does not exist or 
#' is not set, an error message is returned.
#' 
list_sdmpredictors = function(path = options("sdmpredictors_datadir")){
  if (is.null(path)){
    stop("please set 'sdmpredictors_datadir' option first")
  }
  
  #' List SDM Predictors
  #'
  #' This function lists the names of files in the specified directory, splits
  #' them by the period (`.`) character, and returns the base name of each file.
  #'
  #' @param path A character vector of paths to directories where the SDM predictor
  #'   files are stored. Defaults to the `sdmpredictors_datadir` option set in the
  #'   R options. If not set, an error will be raised. The path is assumed to point
  #'   to a valid directory containing files to list.
  #' @return A character vector containing the base names of the files in the
  #'   specified directory (without extensions).
   ff = list.files(path[[1]], full.names = FALSE) |>
    strsplit(".", fixed = TRUE)
  sapply(ff, "[", 1)
}

#' Read a list of rasters as stars
#' 
#' @param layercodes chr one or more layer codes
#' @param datadir chr path to the data directory
#' @param bb NULL or some object from which an st_bbox can be derived.  Thge output is 
#'   cropped to this extent. Ignored if NULL.
#' @param bind logical, if TRUE bind the attributes (variables) if possible
#' @return stars 
read_sdmpredictors = function(layercodes = list_sdmpredictors()[1:5], 
                              datadir = options("sdmpredictors_datadir")[[1]],
                              bb = NULL,
                              bind = TRUE){
  
  
  if (is.null(datadir)){
    stop("please set 'sdmpredictors_datadir' option first")
  }
  pretty = pretty_names()
  
  ff = list.files(datadir, full.names = TRUE)
  
  #' Process and Load Environmental Data Layers
  #'
  #' This function processes environmental data layers by reading them from files,
  #' cropping them if necessary, and optionally combining them into a single object.
  #'
  #' @param layercodes A character vector containing the layer codes corresponding
  #'   to the environmental data layers that need to be processed.
  #' @param ff A character vector containing file paths or names where the environmental
  #'   data layers are stored. These files are read into R using the `stars::read_stars` function.
  #' @param pretty A named character vector mapping layer codes to human-readable
  #'   names for the data layers.
  #' @param bb A bounding box to crop the environmental layers. This should be an object
  #'   of class `sfc` or `bbox` from the `sf` package. If `NULL`, no cropping is done.
  #' @param bind A logical value indicating whether the processed layers should be combined
  #'   into a single object. If `TRUE`, the layers are combined using `do.call(c, ...)`.
  #'   If `FALSE`, the layers are returned as a list.
  #' @return A list or a single object (if `bind = TRUE`) containing the processed environmental
  #'   data layers. The layers are read from the specified files and optionally cropped and
  #'   combined.
  xx = lapply(seq_along(layercodes),
              function(i){
                layercode = layercodes[i]
                ix = grep(layercode, ff, fixed = TRUE)
                if (length(ix) > 0){
                  f = ff[[i]]
                  if (grepl(".zip", f, fixed = TRUE)){
                    f = paste0("/vsizip/", f)
                  }
                  x = stars::read_stars(f) |>
                    rlang::set_names(pretty[layercode])
                  if (!is.null(bb[1])) x <- sf::st_crop(x, bb)
                  x
                }
              })
  
  if (bind){
    xx = try(do.call(c, append(xx, list(along = NA_integer_))))
  }
  
  xx
}


crop_terrain = function(x = c("aspect", "slope", "bathymetry", "terrain_ruggedness"), save = TRUE, bb = get_poly()){
  ss = lapply(x, 
         function(name) {
           cat(name, "\n")
            filename = paste0("~/Desktop/Thesis_data/Biooraclev3/NES/Terrain/NES/raw_terrain", "/", name, ".nc")
            s = read_stars(filename) |> st_set_crs(4326) |> dplyr::slice("time", 1) |> st_crop(bb, as_points = FALSE)
            if(save == TRUE){
                ofilename = paste0("~/Desktop/Thesis_data/Biooraclev3/NES/Terrain/NES/Cropped_terrain", "/", name, ".tif")
                stars::write_stars(s, ofilename)
              } 
              return(s)
              })
  return(ss)
} 

#read_terrain = function(path = "~/Desktop/Thesis_data/Biooraclev3/NES/Terrain/NES/Cropped_terrain") {
 # ff = list.files(path, full.names = TRUE)
  #bnames = basename(ff)
  #bnames = gsub(".tif", "", bnames, fixed = TRUE)
 # s = stars::read_stars(ff) |> rlang::set_names(bnames)
 # return(s)
#}
