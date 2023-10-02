library(tidyverse)
library(readr)

setwd("./data-raw/")

## Subject-Course Codes ----
##_ FS20 ----
SubjectCourse.codes.FS20 <- readr::read_csv(file="./datasets_FS20-Subject-Course_CODES_College.csv",
                                            col_types="cccccccicccccccic") |>
  select(-tlphn_id) |>
  rename("index"="number")
usethis::use_data(SubjectCourse.codes.FS20, overwrite=TRUE)

setwd("..")
