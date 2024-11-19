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
                  "readr",
                  "dplyr",
                  "mgcv",
                  "mgcViz",
                  "rlang"
)
ix = cran_packages %in% installed
for (package in cran_packages[!ix]) {
  install.packages(package)
}
suppressPackageStartupMessages({
  for (package in cran_packages) library(package, character.only = TRUE)
})

THESIS_DATA = "../Thesis_data"
options(sdmpredictors_datadir = file.path(THESIS_DATA, "SDMpredictors"))

ff = list.files("functions", full.names = TRUE)
for(f in ff) {
  source(f, echo = FALSE)
}
