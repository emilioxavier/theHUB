library(tidyverse)
library(readxl)


## institutions of interest dataset ----
MSUpeers <- readxl::read_excel(path="data-raw/theHUB_datasets.xlsx",
                               sheet="MSUpeers")



## Western Michigan University peer institutions ----
WMUpeers <- readxl::read_excel(path="data-raw/theHUB_datasets.xlsx",
                               sheet="WMUpeers")


## save datasets ----
usethis::use_data(MSUpeers, WMUpeers,
                  internal=FALSE, overwrite=TRUE)
