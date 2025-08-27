installed = rownames(installed.packages())

cran_packages = c("tidyverse", 
                  "yaml",
                  "raster",
                  "robis", 
                  "stars", 
                  "dismo", 
                  "terra", 
                  "maxnet",
                  "ggplot2",
                  "leaflet", 
                  "leafem",
                  "lubridate",
                  "biooracler",
                  "biooracle",
                  "readr",
                  "dplyr",
                  "mgcv",
                  "mgcViz",
                  "rlang",
                  "tidysdm",
                  "sf",
                  "ranger",
                  "xgboost",
                  "workflows",
                  "dplyr",
                  "rnaturalearth",
                  "readxl",
                  "corrplot",
                  "performance",
                  "tidyr",
                  "tidymodels",
                  "pROC",
                  "ggridges",
                  "tools",
                  "gbm",
                  "broom.mixed",
                  "dotwhisker",
                  "gganimate",
                  "themis",
                  "purrr",
                  "vip"
                )

ix = cran_packages %in% installed
for (package in cran_packages[!ix]) {
  install.packages(package)
}

#' Suppress Startup Messages While Loading Packages
#'
#' This function loads a list of CRAN packages without displaying startup messages. 
#' It is typically used when you want to load several packages and suppress the 
#' usual startup messages that are printed when a package is loaded.
#' @param cran_packages A character vector containing the names of CRAN packages 
#'   to be loaded. Each package name should be specified as a string (e.g., `"dplyr"`, `"ggplot2"`).
#' @return This function does not return a value. It loads the specified CRAN packages 
#'   into the R session without showing any startup messages.
suppressPackageStartupMessages({
  for (package in cran_packages) library(package, character.only = TRUE)
})

THESIS_DATA = "../Thesis_data"
options(sdmpredictors_datadir = file.path(THESIS_DATA, "SDMpredictors"))

#' Source All R Functions from a Directory
#'
#' This function sources all R scripts located in a specified directory. It iterates over 
#' all the files in the given directory and sources each one into the R session.
#' This is typically used to load a collection of R functions stored in separate files.
#' @param directory A character string specifying the path to the directory containing 
#'   the R scripts to be sourced. If not specified, it defaults to the "functions" directory.
#' @return This function does not return a value. It loads the R scripts from the specified 
#'   directory into the R environment.
ff = list.files("functions", full.names = TRUE)
for(f in ff) {
  source(f, echo = FALSE)
}

path = biooracle_path("NES") |> make_path()
db = read_database(path) |> print()