library(tidyverse)
library(readr)
library(WriteXLS)

setwd("./data-raw/")

## Slate column names ----
slate.column.names.ORIG <- readr::read_csv(file="Slate_column-names.csv",
                                      col_types=c("ccc")) |>
  mutate(index=row_number())

slate.nrow.ORIG <- nrow(slate.column.names.ORIG)
slate.column.names <- dplyr::distinct(slate.column.names.ORIG,
                                      slate.original, slate.new,
                                      .keep_all=TRUE) |>
  select(-index)
slate.nrow.DISTINCT <- nrow(slate.column.names)

if(slate.nrow.ORIG != slate.nrow.DISTINCT) {
  message("There are duplicate rows in ORIGINAL Slate column names. If you recently updated the Slate_column-names.csv file, please note the duplicate row(s).")

  print(mutate(slate.column.names.ORIG, dup.row=duplicated(slate.original)) |> filter(dup.row==TRUE))

}


## financial aid awards ----
MSU.AidAwards <- readr::read_csv(file="MSU_AidFunds.csv",
                                 col_types=c("cccccc")) |>
  rename("fund.code"="FUND_CODE",
         "fund.name"="FUND_NAME",
         "item.type"="ITEM_TYPE",
         "source.description"="SOURCE_DESCRIPTION",
         "category"="CATEGORY") |>
  mutate(src.cat=case_when(grepl(pattern="^honor", x=fund.name, ignore.case=TRUE)~paste0(src.cat, "_HON"),
                           .default=as.character(src.cat))
         )

## save datasets ----
usethis::use_data(slate.column.names, MSU.AidAwards,
                  internal=FALSE, overwrite=TRUE)

setwd("..")
