library(tidyverse)

setwd("./data-raw/")

dataset.files <- list.files(path=".", pattern="CONSTRUCT_dataset")

for( curr.file in dataset.files ) {

  message(paste0("Constructing the dataset associated with ", curr.file, "\n"))
  source(file=curr.file)
  message(paste0("Dataset constructed.\n\n"))
}

message("Done constructing theHUB datasets and constants.\n\n")

setwd("..")
