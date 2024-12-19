set.seed(13)
ds1.nRows <- 10
ds2.nRows <- 5
ds1 <- ds1.mod <- tibble(Uniq.ID=paste0("ID.", 1:ds1.nRows),
                         DATE=lubridate::as_date(lubridate::as_date("2024-12-01"):lubridate::as_date("2024-12-10")),
                         score=sample(x=1:100, size=ds1.nRows, replace=TRUE),
                         dist=(sample(x=1:10, size=ds1.nRows, replace=TRUE) * runif(n=ds1.nRows)),
                         attend=sample(x=c(TRUE, FALSE), size=ds1.nRows, replace=TRUE),
                         workstation=sample(x=c("Linux", "macOS", "Windows"), size=ds1.nRows, replace=TRUE),
                         eyes=sample(x=c("blue", "brown", "green", "hazel"), size=ds1.nRows, replace=TRUE),
                         hair=NA)

ds2 <- tibble(Uniq.ID=paste0("ID.", (ds1.nRows+1):(ds1.nRows+ds2.nRows)),
              DATE=lubridate::as_date(lubridate::as_date("2024-12-11"):lubridate::as_date("2024-12-15")),
              score=sample(x=1:100, size=ds2.nRows, replace=TRUE),
              dist=(sample(x=1:10, size=ds2.nRows, replace=TRUE) * runif(n=ds2.nRows)),
              attend=sample(x=c(TRUE, FALSE), size=ds2.nRows, replace=TRUE),
              workstation=sample(x=c("Linux", "macOS", "Windows"), size=ds2.nRows, replace=TRUE),
              hair=sample(x=c("black", "blond", "brown", "grey", "none"), size=ds2.nRows, replace=TRUE))

ds1.mod[2, "DATE"] <- NA
ds1.mod[4, "DATE"] <- as_date(lubridate::as_date("2024-12-31"))
ds1.mod[2, "score"] <- 100L
ds1.mod[3, "score"] <- NA
ds1.mod[2, "attend"] <- FALSE
ds1.mod[9, "attend"] <- NA
ds1.mod[5, "dist"] <- NA
ds1.mod[7, "dist"] <- 13.26
ds1.mod[6, "workstation"] <- "Linux"
ds1.mod[8, "workstation"] <- NA

ds.simple <- bind_rows(ds1, ds2)
ds.complex <- bind_rows(ds1.mod, ds2)

ds2 <- ds.simple
ds2 <- ds.complex

ds1.name <- "original"
ds2.name <- "oct292024"
UniqID.colName <- "Uniq.ID"

dataset.compare <- function(ds1, ds1.name, ds2, ds2.name, UniqID.colName="Uniq.ID") {

  ## make all the data characters ----
  ds1 <- mutate(ds1, across(everything(), ~as.character(.x))) |>
    mutate(across(everything(), ~replace_na(.x, replace="NA")))
  ds2 <- mutate(ds2, across(everything(), ~as.character(.x))) |>
    mutate(across(everything(), ~replace_na(.x, replace="NA")))

  ## same datasets? ----
  same.datasets.TF <- dplyr::setequal(x=ds1, y=ds2)
  ##_ message ----
  if (same.datasets.TF==TRUE) {
    stop(" <><>>> The datasets are the same!! No need for a comparison!!\n\n")
  } else {
    message(" [x|x] The datasets are different... Let's find out how!")
  }

  ## dataset dimensions ----
  ds1.dim <- dim(ds1)
  ds1.colNames <- colnames(ds1)
  ds2.dim <- dim(ds2)
  ds2.colNames <- colnames(ds2)
  ## common columns (based on name) ----
  ds1.colNames.tb <- tibble(colNames=ds1.colNames)
  ds2.colNames.tb <- tibble(colNames=ds2.colNames)
  ds.common.colNames <- inner_join(x=ds1.colNames.tb, y=ds2.colNames.tb, by="colNames") |>
    pull(colNames)

  ##_ message ----
  if ( ds1.dim[2] != ds2.dim[2] ) {
    message(" [x|x] The datasets have different number of columns!! Retaining the following columns:")
    message("  ")
  }

  ## common UniqIDs ----
  ds1.IDs <- select(ds1, {{UniqID.colName}})
  ds2.IDs <- select(ds2, {{UniqID.colName}})

  common.IDs <- inner_join(x=ds1.IDs, y=ds2.IDs, by={{UniqID.colName}})
  dsNEW <- anti_join(x=ds2, y=ds1, by={{UniqID.colName}})
  new.IDs <- select(dsNEW, {{UniqID.colName}})

  ## about datasets ----
  diff.tb <- tibble::tibble(desc=c("n.rows", "n.cols", "n.cols.same.btwn", "n.colNames.same.btwn", "n.rows.NEW"),
                            ds1.name=c(ds1.dim[1], ds1.dim[2], "XXX", "YYY", NA),
                            ds2.name=c(ds2.dim[1], ds2.dim[2], "XXX", "YYY", nrow(dsNEW))
  )

  ## extract rows in previous dataset ----
  ds2.prev <- right_join(x=ds2, y=ds1.IDs, by={{UniqID.colName}})

  ## changes between current & past ----
  ds1.VS.ds2.equal.TF <- dplyr::setequal(x=ds1, y=ds2.prev)
  ds1.VS.ds2.TF <- tibble::as_tibble(ds1 != ds2.prev)

  ##_ datasets summaries ----
  ds1.VS.ds2.nRows.diff <- rowSums(ds1.VS.ds2.TF, na.rm=TRUE)
  ds1.VS.ds2.nCols.diff <- colSums(ds1.VS.ds2.TF, na.rm=TRUE)

  ##_ rows || changes between current & past ----
  nRows.ds1 <- nrow(ds1)
  nRows.diff.ds1 <- sum(ds1.VS.ds2.nRows.diff > 0, na.rm=TRUE)
  nRows.ds2 <- nrow(ds2)
  nRows.NEW <- nrow(new.ds)

  ##_ columns || changes between current & past ----
  nCols.diff.ds1 <- sum(ds1.VS.ds2.nCols.diff > 0, na.rm=TRUE)
  cols.diff.ds1.SUMMARY <- tibble::as_tibble(ds1.VS.ds2.nCols.diff) |>
    rename(diff.n=value) |>
    mutate(diff.n=as.integer(diff.n),
           diff.pct=diff.n/nRows.ds1,
           diff.TF=diff.n>0) |>
    tibble::add_column(column.name=names(ds1.VS.ds2.nCols.diff), .before=1) |>
    select(column.name, diff.TF, diff.n, diff.pct) |>
    filter(column.name!={{UniqID.colName}}) |>
    rename_with(.cols=starts_with("diff"), ~paste(ds2.name, .x, sep="."))


}
