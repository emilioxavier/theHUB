## ----setup, echo=FALSE, eval=FALSE, include=FALSE-----------------------------
#  knitr::opts_chunk$set(
#    collapse = TRUE,
#    comment = "#>"
#  )
#  library(knitr)
#  library(kableExtra)

## ----loadPackages, eval=FALSE, include=TRUE-----------------------------------
#  library(theHUB)
#  library(ISOcodes)
#  library(priceR)

## ----ExtractCountry, eval=FALSE, include=TRUE---------------------------------
#  country.iso <- as_tibble(ISOcodes::ISO_3166_1) |>
#    mutate(Name.ISO=Name)

## ----ExtractCurrency, eval=FALSE, include=TRUE--------------------------------
#  country.currencies <- priceR::currencies() |>
#    as_tibble() |>
#    separate(col="code", into=c("Alpha_2", "currency.DROP"), remove=FALSE, sep="[[:alpha:]]{1}$") |>
#    select(-currency.DROP) |>
#    rename("currency"="description",
#           "currency.code"="code")

## ----MergeEverything, eval=FALSE, include=TRUE--------------------------------
#  country.DATA <- full_join(x=country.iso, y=country.currencies, by="Alpha_2") |>
#    full_join(y=SLATE.country.names, by="Name")

## ----WriteXLSX, eval=FALSE, include=TRUE--------------------------------------
#  WriteXLS::WriteXLS(x="country.DATA",
#                     ExcelFileName="CountryISO-slate_matches.xlsx",
#                     FreezeRow=1)

## ----loadPackages2, include=FALSE---------------------------------------------
library(tidyselect)
library(dplyr)
library(tidyr)

## ----theDataset, echo=FALSE---------------------------------------------------
country.currency.DISPLAY <- select(theHUB::country.currency, Alpha_2, Alpha_3, Numeric, Name, Official_name, Name.ISO, currency, currency.code) |>
  mutate(across(where(is.character), ~replace_na(.x, "")))
col.names <- c("Alpha 2", "Alpha 3", "Numeric", "Name", "Official Name", "Name (ISO)", "Currency", "Currency Code")

kableExtra::kbl(country.currency.DISPLAY, booktabs=TRUE, caption=NULL, col.names=col.names) |> 
  kableExtra::kable_styling(latex_options="striped")

