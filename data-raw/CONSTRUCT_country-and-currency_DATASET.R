library(tidyverse)
library(ISOcodes)
library(priceR)
library(WriteXLS)

setwd("data-raw/")

## load SLATE country names ----
load(file="SLATE-country-names_fall2022.RData")

SLATE.country.names.2022 <- mutate(SLATE.country.names.2022,
                                       Name.SLATE=Name,
                                       SLATE.tf=TRUE)

## extract country names and ISO 2- and 3-character abbreviations ----
country.iso <- as_tibble(ISO_3166_1) |>
  mutate(Name.ISO=Name)

## extract country-currency data ----
country.currencies <- currencies() |>
  as_tibble() |>
  separate(col="code", into=c("Alpha_2", "currency.DROP"), remove=FALSE, sep="[[:alpha:]]{1}$") |>
  select(-currency.DROP) |>
  rename("currency"="description",
         "currency.code"="code")

## merge together the datasets ----
country.DATA <- full_join(x=country.iso, y=country.currencies, by="Alpha_2") |>
  full_join(y=SLATE.country.names.2022, by="Name")

# SLATE.matches <- right_join(x=country.iso, y=SLATE.country.names.2022, by="Name")

WriteXLS::WriteXLS(x=c("country.iso", "country.DATA", "SLATE.matches"),
                   ExcelFileName="SLATE-ISO-countryMatches_v02.xlsx",
                   FreezeRow=1)

## merge rows based on Name.ISO and Name.SLATE
## using Wikipedia, determine the currency being used by the countries not automatically matched.

country.currency <- readxl::read_xlsx(path="SLATE-ISO-countryMatches.xlsx", sheet="country.DATA")

setwd("..")
