installed = rownames(installed.packages())

cran_packages = c("tidyverse", 
                  "yaml",
                  "ggplot2",
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
options(DMRtemp_datadir = file.path(THESIS_DATA, "Current_data_files_10_21", "temp_no_zero.csv"))
options(DMRsal_datadir = file.path(THESIS_DATA, "Current_data_files_10_21", "salinity_no_zero.csv"))
options(DMRdepth_datadir = file.path(THESIS_DATA, "Current_data_files_10_21", "depth_no_zero.csv"))

ff = list.files("GAM_functions", full.names = TRUE)
for(f in ff) {
  source(f, echo = FALSE)
}
