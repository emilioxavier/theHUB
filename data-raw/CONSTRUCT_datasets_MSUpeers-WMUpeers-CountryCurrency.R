library(tidyverse)
library(readxl)
library(WriteXLS)

setwd("./data-raw/")

## load post-secondary education dataset ----
load(file="~/OneDrive - Michigan State University/projects/PAG/DATASETS_CLEAN/NCES_HighSchools-and-PostSecondary_DATASET.RData")

PostSecondary.2021.2022 <- filter(PostSecondary, SCHOOLYEAR=="2021-2022") |>
  select(UNITID, LAT, LON)

## institutions of interest dataset ----
MSUpeers <- readxl::read_excel(path="./datasets_MSUpeers-WMUpeers.xlsx",
                               sheet="MSUpeers") |>
  mutate(UNITID=as.character(UNITID)) |>
  left_join(y=PostSecondary.2021.2022, by="UNITID") |>
  select(INSTNM:ZIP, LAT, LON, everything())

## Western Michigan University peer institutions ----
WMUpeers <- readxl::read_excel(path="./datasets_MSUpeers-WMUpeers.xlsx",
                               sheet="WMUpeers") |>
  mutate(UNITID=as.character(UNITID)) |>
  left_join(y=PostSecondary.2021.2022, by="UNITID") |>
  select(INSTNM:ZIP, LAT, LON, everything())

## country and currency dataset ----
country.currency <- readxl::read_excel(path="./datasets_CountryAndCurrency_spring2023.xlsx",
                                       sheet="country.DATA") |>
  filter(!is.na(Alpha_2)) |>
  select(Alpha_2, Alpha_3, Alpha_3.shape, Numeric, Name, Name.SLATE, Name.ggplot, Name.shape, Name.ISO, Official_name:currency.code)

usethis::use_data(country.currency,
                  internal=FALSE, overwrite=TRUE)

## save datasets ----
usethis::use_data(MSUpeers, WMUpeers, country.currency,
                  internal=FALSE, overwrite=TRUE)

WriteXLS::WriteXLS(x=c("MSUpeers", "WMUpeers"),
                   ExcelFileName="./datasets_PeerInstitution_DoNotEdit.xlsx",
                   SheetNames=c("MSUpeers", "WMUpeers"),
                   FreezeRow=1, FreezeCol=1)

setwd("..")
