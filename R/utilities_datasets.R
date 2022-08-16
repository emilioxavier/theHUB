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
dataset.summary <- function(dataset, ExcelFileName, n.examples=4, overwriteXLS=FALSE) {

  ## check for exisiting Excel file ----
  file.exists.tf <- file.exists(ExcelFileName)
  if (file.exists(ExcelFileName) & (overwriteXLS==FALSE)) {
    stop("Provided Excel workbook already exists!")
  }

  ## basic information ----
  ds.colNames <- colnames(dataset)
  n.rows <- nrow(dataset)

  ## determine column types ----
  ds.colTypes <- dplyr::summarise_all(dataset, class) |>
    t() |>
    as.data.frame() |>
    tibble::rownames_to_column(var="column.name") |>
    dplyr::rename("colType.1"="V1",
                  "colType.2"="V2") |>
    dplyr::mutate(colType.diff=case_when(colType.1!=colType.2~"CHECK",
                                         TRUE~"")) |>
    tibble::as_tibble()

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
  Admiss.examples <- dplyr::bind_cols(ds.colTypes, ds.examples) |>
    tibble::add_column(colname.new=NA,
                       coltype.new=NA)

  ## write out to Excel ----
  WriteXLS::WriteXLS(x=Admiss.examples,
                     ExcelFileName=ExcelFileName,
                     FreezeRow=1, FreezeCol=8)

  ## return examples ----
  return(Admiss.examples)

}

