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
                  "rlang",
                  "readr",
                  "readxl",
                  "corrplot"
)
ix = cran_packages %in% installed
for (package in cran_packages[!ix]) {
  install.packages(package)
}
suppressPackageStartupMessages({
  for (package in cran_packages) library(package, character.only = TRUE)
})

