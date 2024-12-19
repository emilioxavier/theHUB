library(tidyverse)
library(readr)

## >> script updated 26/aug/2024 << ----


setwd("./data-raw/")

## read in state name - abbreviation data ----
USCan.NameAbbr <- readr::read_csv(file="datasets_US-Canada-name-abbreviation.csv")

## >>> save datasets <<< ----
usethis::use_data(USCan.NameAbbr,
                  internal=FALSE, overwrite=TRUE)

setwd("..")

message("\n..::||>>>>> The script is done!!! <<<<<||::..\n")
