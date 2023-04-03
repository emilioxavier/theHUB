library(readxl)
library(tidyverse)

setwd("data-raw/")

## read in Princeton Review data ----
SAT.ACT.PR <- readxl::read_excel(path="datasets_SAT-ACT_scoreConversion_PrincetonReview_spring2022.xlsx") |>
  mutate(SAT=as.integer(SAT),
         ACT=as.integer(ACT))

## construct SAT score range ----
SAT.min <- min(SAT.ACT.PR$SAT)
SAT.max <- max(SAT.ACT.PR$SAT)

SAT.tb <- tibble(SAT.int=seq.int(from=SAT.max, to=SAT.min, by=-1L))

## create conversion tables ----
##_ SAT to ACT ----
SAT.2.ACT <- full_join(x=SAT.ACT.PR, y=SAT.tb, by=c("SAT"="SAT.int")) |>
  arrange(desc(SAT)) |>
  fill(ACT, .direction="up")

##_ ACT to SAT ----
## each integer, sequential SAT score is used to calculate the
## mean SAT score for the corresponding ACT score. this results in equivalent SAT
## scores that are five points greater (with the exception of 1600 == 36) than
## using the three or four SAT values provided in the Princeton Review table.
ACT.2.SAT <- group_by(SAT.2.ACT, ACT) |>
  mutate(SAT=as.integer(ceiling(mean(SAT)))) |>
  distinct(ACT, .keep_all=TRUE)

# ACT.2.SAT <- group_by(SAT.ACT.PR, ACT) |>
#   mutate(SAT=as.integer(mean(SAT))) |>
#   distinct(ACT, .keep_all=TRUE)

## save datasets ----
usethis::use_data(SAT.2.ACT, ACT.2.SAT, SAT.ACT.PR,
                  internal=FALSE, overwrite=TRUE)

setwd("..")
