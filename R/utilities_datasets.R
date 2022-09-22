#' @title Extract Unique Values from Each Column
#'
#' @description Given a `tibble` or `data.frame` extract unique examples for
#'   each column. This function is not publically available, but maybe a future
#'   version will be.
#'
#' @param dataset `tibble` or `data.frame` of interest
#' @param cell.blank.tf constructed within [theHUB::dataset.summary()] using the
#'   command `purrr::map_dfc(dataset, is.BLANK)` and returns a tibble the same
#'   dimensions as `dataset` and indicates whether or not each cell is blank.
#'   `TRUE` indicates the cell is blank and `FALSE` indicates it contains some
#'   type of value. `NA`s are [dplyr::coalesce()]d into `FALSE`.
#' @param cell.NA.tf constructed within [theHUB::dataset.summary()] using the
#'   command `purrr::map_dfc(dataset, is.na)` and returns a tibble the same
#'   dimensions as `dataset` and indicates whether or not each cell is blank.
#'   `TRUE` indicates the cell contains a `NA` and `FALSE` indicates it contains some
#'   type of value.
#' @param size integer value indicating the number examples to return
#'
#' @return tibble of examples
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
extract.unique <- function(dataset, cell.blank.tf, cell.NA.tf, size=3) {

  ## general information ----
  n.cols <- ncol(dataset)

  ## construct empty tibble to populate ----
  CoI.report.df <- matrix(data=NA, nrow=(size+1), ncol=n.cols) |>
    as.data.frame()
  colnames(CoI.report.df) <- colnames(dataset)
  CoI.report <- as_tibble(CoI.report.df)

  ## extract examples from each columns
  for (curr.col in seq_len(length.out=n.cols)) {

    CoI.data <- dplyr::pull(dataset, var=curr.col)
    CoI.blank <- dplyr::pull(cell.blank.tf, var=curr.col)
    CoI.data.noBLANK <- CoI.data[!CoI.blank]
    CoI.data.clean <- CoI.data.noBLANK[!is.na(CoI.data.noBLANK)]
    CoI.data.unique <- unique(CoI.data.clean)
    n.examples <- length(CoI.data.unique)
    n.missing <- 0L
    if (n.examples < size) {
      n.missing <- size - n.examples
    }

    ##_ what if there are not enough examples ----
    if (n.missing == 0) {
      CoI.examples <- c(n.examples,
                        sample(x=CoI.data.unique, size=size))
    } else {
      CoI.examples <- c(n.examples,
                        sample(x=CoI.data.unique, size=n.examples),
                        rep_len(x=NA, length.out=n.missing))
    }

    ##_ add examples to column of interest ----
    CoI.report[, curr.col] <- CoI.examples

  }

  return(CoI.report)

}


#' @title Find Duplicate Columns
#'
#' @description Identifies and returns duplicate columns. The resulting data.frame
#'   includes the following:#'
#'   - column.name: column name
#'   - duplicate: logical indicating if the column is duplicate. FYI: only duplicate
#'   columns are returned
#'   - duplicate.colName: duplicate columns
#'
#'  _**Not a publicly available function at this time.**_
#'
#' @param data `tibble` or `data.frame` of interest
#' @param data.md5s the calculated md5 hashes
#'
#' @return data.frame with the above information
#'
#' @examples
#' data <- tibble(first.name=c("Alice", "Bob", "Carl", "Debbie"),
#'                   last.name=c("Masters", "Roberts", "Roberts", "Smith"),
#'                   pref.name=c("Alice", "Bob", "Carl", "Debbie"),
#'                   role=c("data", "data", "coding", "data"),
#'                   job=c("data", "data", "coding", "data"),
#'                   expert=c("data", "data", "coding", "data"),
#'                   building=c("Chemistry", "Biochemistry", "Chemistry", "Statistics"))
#' find.duplicate.cols(data=data)
#' #  column.name duplicate.tf    duplicate.colNames
#' #1  first.name         TRUE first.name, pref.name
#' #2   pref.name         TRUE first.name, pref.name
#' #3        role         TRUE     role, job, expert
#' #4         job         TRUE     role, job, expert
#' #5      expert         TRUE     role, job, expert
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
find.duplicate.cols <- function(data, data.md5s) {

  ## determine the pairwise duplicates ----
  pairwise.dup <- purrr::map2(data.md5s, data.md5s,
                              ~which(data.md5s == .x & data.md5s == .y))

  ## retain only columns with duplicates ----
  data.pairs <- pairwise.dup[sapply(pairwise.dup, length) > 1]

  ## construct the summary ----
  ## i am not really sure how it works, but the transpose |> as_tibble |> transpose |> as.data.frame
  ## results in a data.frame with the duplicate column names as a string in a
  ## single column
  data.pairs.summary <- sapply(data.pairs, names) |>
    t() |>
    tibble::as_tibble() |>
    t() |>
    as.data.frame() |>
    tibble::rownames_to_column(var="column.name") |>
    dplyr::mutate(duplicate.tf=TRUE) |>
    dplyr::rename("duplicate.colNames"="V1") |>
    dplyr::select(column.name, duplicate.tf, duplicate.colNames)

  ## return results ----
  return(data.pairs.summary)

}


#' @title Dataset Summary
#'
#' @description When working with new datasets it is helpful to know what type
#'  of data is included and easily retype the columns and provide new column
#'  names. This function identifies the type of data in each column and provides
#'  unique examples data within each column. The results are written to an Excel
#'  workbook. The new column names and re-defined column types can easily be added
#'  to the Excel workbook, imported into R, and assigned to the original dataset.
#'  The goal is to reduce the logistical burden of the user.
#'
#'  This function will likely evolve overtime.
#'
#'  _**Re-running this command will overwrite previous versions of the file!!**_
#'
#' @param dataset `tibble` or `data.frame` of interest
#' @param ExcelFileName string indicating the Excel workbook filename. The value
#'   is passed to [WriteXLS::WriteXLS()]. _**Re-running this command will overwrite
#'   previous versions of the file!!**_
#' @param n.examples integer value indicating the number examples to return. Passed
#'   to `size` of [theHUB::extract.unique()].
#' @param overwriteXLS logical to overwrite existing Excel workbook; default is `FALSE`
#' @param group.same.cols logical indicating if the columns should be grouped by
#'   those with the same information.
#'
#' @return tibble of column names, types, and examples
#' @export
#'
#' @examples
#' dataset.summary(dataset=ds.orig,
#'                 ExcelFileName="ds_Column-names-and-data-types-and-examples.xlsx",
#'                 n.examples=4)
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
dataset.summary <- function(dataset, ExcelFileName, n.examples=4, overwriteXLS=FALSE, group.same.cols=TRUE) {

  ## check for existing Excel file ----
  file.exists.tf <- file.exists(ExcelFileName)
  if (file.exists(ExcelFileName) & (overwriteXLS==FALSE)) {
    stop("Provided Excel workbook already exists!")
  }

  ## basic information ----
  ds.colNames <- colnames(dataset)
  n.rows <- nrow(dataset)

  ## construct the md5 hashes ----
  dataset.md5s <- sapply(dataset, digest::digest)

  ## find duplicate columns ----
  ds.duplicate.cols <- find.duplicate.cols(data=dataset, data.md5s=dataset.md5s)

  ## determine column types ----
  ds.colTypes <- dplyr::summarise_all(dataset, class) |>
    t() |>
    as.data.frame() |>
    tibble::rownames_to_column(var="column.name") |>
    dplyr::mutate(col.idx=row_number()) |>
    dplyr::rename("colType.1"="V1",
                  "colType.2"="V2") |>
    dplyr::mutate(colType.diff=case_when(colType.1!=colType.2~"CHECK",
                                         TRUE~""),
                  col.idx=row_number() ) |>
    tibble::as_tibble() |>
    tibble::add_column(md5.hash=dataset.md5s) |>
    dplyr::select(column.name, col.idx,
                  colType.1, colType.2, colType.diff, md5.hash)

  ## is blank? ----
  cell.blank.tf <- purrr::map_dfc(dataset, is.BLANK)
  col.blank.n <- colSums(cell.blank.tf, na.rm=TRUE)

  ## is NA? ----
  cell.NA.tf <- purrr::map_dfc(dataset, is.na)
  col.NA.n <- colSums(cell.NA.tf, na.rm=TRUE)

  ## add columns indicating number of blanks and NAs ----
  ds.colTypes <- tibble::add_column(ds.colTypes,
                                    n.TOT=n.rows,
                                    n.BLANK=col.NA.n,
                                    n.NA=col.NA.n)

  ## extract examples ----
  ds.examples <- extract.unique(dataset=dataset,
                                cell.blank.tf=cell.blank.tf,
                                cell.NA.tf=cell.NA.tf,
                                size=n.examples) |>
    t()

  ## create and apply example names ----
  example.names <- paste("example.", seq_len(length.out=n.examples), sep="")
  colnames(ds.examples) <- c("n.unique", example.names)
  ds.examples <- as_tibble(ds.examples)

  ## create dataset for export ----
  ds.summary <- dplyr::bind_cols(ds.colTypes, ds.examples) |>
    tibble::add_column(colname.new=NA,
                       coltype.new=NA)

  ## add in duplicate column information ----
  ds.summary <- dplyr::left_join(x=ds.summary, y=ds.duplicate.cols,
                                 by=c("column.name")) |>
    select(column.name:n.unique, duplicate.tf, duplicate.colNames, example.1:coltype.new)

  ## sort on md5s ----
  if (group.same.cols == TRUE) {
    ds.summary <- arrange(ds.summary, md5.hash, col.idx)
  }

  ## write out to Excel ----
  WriteXLS::WriteXLS(x=ds.summary,
                     ExcelFileName=ExcelFileName,
                     FreezeRow=1, FreezeCol=11)

  ## return examples ----
  return(ds.summary)

}


