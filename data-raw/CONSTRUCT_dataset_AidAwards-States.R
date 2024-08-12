library(tidyverse)
library(readr)
library(WriteXLS)

setwd("./data-raw/")

## financial aid awards ----
MSU.AidAwards <- readr::read_csv(file="datasets_MSU_AidFunds.csv",
                                 col_types=c("cccccc")) |>
  rename("fund.code"="FUND_CODE",
         "fund.name"="FUND_NAME",
         "item.type"="ITEM_TYPE",
         "source.description"="SOURCE_DESCRIPTION",
         "category"="CATEGORY") |>
  mutate(src.cat=case_when(grepl(pattern="^honor", x=fund.name, ignore.case=TRUE)~paste0(src.cat, "_HON"),
                           .default=as.character(src.cat))
         )

## US & Canada abbreviations ----
US.Canada.name.abbreviation <- readr::read_csv(file="datasets_US-Canada-name-abbreviation.csv",
                                               col_types=c("cc"))


## save datasets ----
usethis::use_data(MSU.AidAwards, US.Canada.name.abbreviation,
                  internal=FALSE, overwrite=TRUE)

setwd("..")
