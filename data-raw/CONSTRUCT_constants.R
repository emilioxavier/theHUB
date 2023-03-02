library(tidyverse)


## MSU colour palettes ----
msu.darkGreen <- "#18453b"

msu.darkGreenTints <- c(
  "#18453b",
  "#2f574e",
  "#466a62",
  "#5d7c75",
  "#748f89",
  "#8ba29d",
  "#a2b4b0",
  "#b9c7c4",
  "#d0d9d7",
  "#e7eceb"
)

msu.palette <- c(
  "#18453b",  ## msu green
  "#0db14b",  ## kelly green
  "#97a2a2",  ## grey
  "#f08521",  ## orange
  "#008183",  ## teal
  "#909ab7",  ## blue-grey
  "#535054",  ## dark grey
  "#d1de3f",  ## yellow-green
  "#e8d9b5",  ## cream
  "#c89a58",  ## texas-brown
  "#94ae4a",  ## split pea soup green
  "#6e005f",  ## eggplant
  "#cb5a28"   ## sienna
)

msu.heatmap <- colorRampPalette(colors=c("#e7eceb", msu.darkGreen), bias=1,
                                space="rgb", interpolate="linear", alpha=FALSE)

msu.heatmap.20 <- tibble::tibble(hex=msu.heatmap(22),
                                 start=c(0, seq(from=1, to=100, by=5), 100),
                                 stop=c(0, seq(from=5, to=95, by=5), 99, 100))

msu.heatmap.100 <- tibble::tibble(hex=msu.heatmap(101),
                                  pct=as.integer(seq(from=0, to=100, by=1)))

usethis::use_data(msu.darkGreen, msu.darkGreenTints, msu.palette,
                  msu.heatmap.20, msu.heatmap.100,
                  internal=FALSE, overwrite=TRUE)


## University of Wisconsin colour palettes ----
wisc.badgerRed <- "#da004c"
wisc.white <- "#ffffff"

wisc.badgerRedTints <- c(
  "#da004c",
  "#dd195d",
  "#e1326f",
  "#e54c81",
  "#e86693",
  "#ec7fa5",
  "#f099b7",
  "#f3b2c9",
  "#f7ccdb",
  "#fbe5ed"
)

wisc.secondary <- c(
  "#a1002f",  ## dark red
  "#8b0037",  ## deep red (magenta?)
  "#e6e6e6",  ## grey10
  "#b3b3b3",  ## grey30
  "#737373",  ## grey55
  "#000000"   ## black
)
wisc.accent <- c(
  "#ff8000",  ## orange
  "#ffbf00",  ## orange-yellow
  "#e8dfa7",  ## tan
  "#97b85f",  ## avocado green
  "#6b9999",  ## blue-grey
  "#386666"   ## dark teal
  )

wisc.heatmap <- colorRampPalette(colors=c("#fbe5ed", wisc.badgerRed), bias=1,
                                space="rgb", interpolate="linear", alpha=FALSE)

wisc.heatmap.20 <- tibble::tibble(hex=wisc.heatmap(22),
                                 start=c(0, seq(from=1, to=100, by=5), 100),
                                 stop=c(0, seq(from=5, to=95, by=5), 99, 100))

wisc.heatmap.100 <- tibble::tibble(hex=wisc.heatmap(101),
                                  pct=as.integer(seq(from=0, to=100, by=1)))

usethis::use_data(wisc.badgerRed, wisc.white,
                  wisc.badgerRedTints,
                  wisc.secondary, wisc.accent,
                  wisc.heatmap.20, wisc.heatmap.100,
                  internal=FALSE, overwrite=TRUE)


## calculating distance on a globe ----
deg2rad <- pi/180
earth.radius.miles <- 3963.17
earth.radius.km <- 6373

usethis::use_data(deg2rad, earth.radius.miles, earth.radius.km,
                  internal=FALSE, overwrite=TRUE)

## keywords ----
keywords <- readr::read_csv(file="./data-raw/keywords_areas.csv", trim_ws=FALSE)
usethis::use_data(keywords, overwrite=TRUE)
# keywords.tb <- group_by(keywords, area) |> summarise(query=paste(keyword, collapse="|"))

## Subject-Course Codes ----
##_ FS20 ----
SubjectCourse.codes.FS20 <- readr::read_csv(file="./data-raw/FS20-Subject-Course_CODES_College.csv",
                                            col_types="cccccccicccccccic") |>
  select(-tlphn_id) |>
  rename("index"="number")
usethis::use_data(SubjectCourse.codes.FS20, overwrite=TRUE)

## Term-Code Translation Table ----
term.translation <- tibble::tibble(abbrev=c(1, 2, 3, 5, 6, 8, 9),
                                   full=c("WinterQ", "Spring", "SpringQ", "Summer", "SummerQ", "Fall", "FallQ"),
                                   short=c("WQ", "SS", "SQ", "US", "UQ", "FS", "FQ"))
usethis::use_data(term.translation, overwrite=TRUE)

