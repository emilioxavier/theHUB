library(tidyverse)
library(readxl)
library(WriteXLS)


## load post-secondary education dataset ----
load(file="~/OneDrive - Michigan State University/projects/PAG/DATASETS_CLEAN/NCES_HighSchools-and-PostSecondary_DATASET.RData")

PostSecondary.2020.2021 <- filter(PostSecondary, SCHOOLYEAR=="2020-2021") |>
  select(UNITID, LAT, LON)

## institutions of interest dataset ----
MSUpeers <- readxl::read_excel(path="./data-raw/datasets_theHUB.xlsx",
                               sheet="MSUpeers") |>
  mutate(UNITID=as.character(UNITID)) |>
  left_join(y=PostSecondary.2020.2021, by="UNITID") |>
  select(INSTNM:ZIP, LAT, LON, everything())

## Western Michigan University peer institutions ----
WMUpeers <- readxl::read_excel(path="./data-raw/datasets_theHUB.xlsx",
                               sheet="WMUpeers") |>
  mutate(UNITID=as.character(UNITID)) |>
left_join(y=PostSecondary.2020.2021, by="UNITID") |>
  select(INSTNM:ZIP, LAT, LON, everything())

## country and currency dataset ----
country.currency <- readxl::read_excel(path="./data-raw/SLATE-ISO-countryMatches.xlsx",
                                       sheet="country.DATA") |>
  filter(!is.na(Alpha_2))


## save datasets ----
usethis::use_data(MSUpeers, WMUpeers, country.currency,
                  internal=FALSE, overwrite=TRUE)

WriteXLS::WriteXLS(x=c("MSUpeers", "WMUpeers"),
                   ExcelFileName="./data-raw/datasets_PeerInstitution.xlsx",
                   SheetNames=c("MSUpeers", "WMUpeers"),
                   FreezeRow=1, FreezeCol=1)
