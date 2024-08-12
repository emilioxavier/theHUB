library(tidyverse)
library(readxl)
library(WriteXLS)

setwd("./data-raw/")

## load post-secondary education dataset ----
load(file="/Volumes/OneDrive-MichiganStateUniversity/projects/PAG/DATASETS_CLEAN/NCES_HighSchools-and-PostSecondary_DATASET.RData")

PostSecondary.2022.2023 <- filter(PostSecondary, SCHOOLYEAR=="2022-2023") |>
  select(UNITID, LAT, LON)

## institutions of interest dataset ----
MSUpeers <- readxl::read_excel(path="./datasets_MSUpeers-WMUpeers.xlsx",
                               sheet="MSUpeers") |>
  mutate(UNITID=as.character(UNITID)) |>
  left_join(y=PostSecondary.2022.2023, by="UNITID") |>
  select(INSTNM:ZIP, LAT, LON, everything())

## Western Michigan University peer institutions ----
WMUpeers <- readxl::read_excel(path="./datasets_MSUpeers-WMUpeers.xlsx",
                               sheet="WMUpeers") |>
  mutate(UNITID=as.character(UNITID)) |>
  left_join(y=PostSecondary.2022.2023, by="UNITID") |>
  select(INSTNM:ZIP, LAT, LON, everything())


## save datasets ----
usethis::use_data(MSUpeers, WMUpeers,
                  internal=FALSE, overwrite=TRUE)

WriteXLS::WriteXLS(x=c("MSUpeers", "WMUpeers"),
                   ExcelFileName="./datasets_PeerInstitution_DoNotEdit.xlsx",
                   SheetNames=c("MSUpeers", "WMUpeers"),
                   FreezeRow=1, FreezeCol=1)

setwd("..")
