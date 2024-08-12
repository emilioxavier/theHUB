library(tidyverse)
library(readxl)
library(WriteXLS)

setwd("./data-raw/")


## country and currency dataset ----
country.currency <- readxl::read_excel(path="./datasets_CountryAndCurrency_spring2023.xlsx",
                                       sheet="country.DATA") |>
  filter(!is.na(Alpha_2)) |>
  select(Alpha_2, Alpha_3, Alpha_3.shape, Numeric, Name, Name.SLATE, Name.ggplot, Name.shape, Name.ISO, Official_name:currency.code)

usethis::use_data(country.currency,
                  internal=FALSE, overwrite=TRUE)


setwd("..")
