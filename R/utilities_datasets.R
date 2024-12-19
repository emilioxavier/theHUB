#' @title Classification Check
#'
#' @description Identify when a new class in the list of **long** responses that
#' need to be converted to their **short**ened forms.
#'
#' @param data `tibble` or `data.frame` of interest
#' @param col.oi string indicating the column with classes of interest
#' @param ct `tibble` or `data.frame` conversion table with a column with the
#'   information to be converted and the information to be converted to
#' @param ct.col string indicating the column to match to the classes in the `col.oi`
#'
#' @return Indicator of missing classifications. See Examples.
#' @export
#'
#' @importFrom dplyr filter pull
#'
#' @examples
#' \dontrun{
#'  classification.check(data=data.oi, col.oi="Animal", ct=ToFrom.ct, ct.col="long")
#'  # No new classes in the dataset of interest.
#'
#'  classification.check(data=data.oi, col.oi="Animal", ct=ToFrom.ct, ct.col="long")
#'  # There are unmatched values in the data of interest:
#'  #  -> Dogs
#'  #  -> Cats
#' }
#'
#' @author Emilio Xavier Esposito \email{emilio.esposito@@gmail.com}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
classification.check <- function(data, col.oi, ct, ct.col) {

  ## pull unique, sorted column of interest ----
  col.oi.values <- dplyr::filter(data, !is.na({{col.oi}})) |>
    dplyr::pull({{col.oi}}) |>
    paste(collapse=", ") |>
    strsplit(split=", ") |>
    unlist() |>
    unique() |>
    sort()

  ## conversion-table column ----
  ct.values <- sort(ct[[ct.col]])

  ## any new classes? ----
  new.class <- col.oi.values[!col.oi.values %in% ct.values]

  ## write out status ----
  if ( any(!is.na(new.class)) ) {
    new.class.list <- paste(" ->", new.class, collapse="\n")
    mess <- paste("There are unmatched values in the data of interest:\n",
                  new.class.list, sep="")
    message(mess)
  } else {
    message("No new classes in the dataset of interest.")
  }

}


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
#' @author Emilio Xavier Esposito \email{emilio.esposito@@gmail.com}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
extract.unique <- function(dataset, cell.blank.tf, cell.NA.tf, size=3) {

  ## general information ----
  n.cols <- ncol(dataset)

  ## construct empty tibble to populate ----
  CoI.report.df <- matrix(data=NA, nrow=(size+1), ncol=n.cols) |>
    as.data.frame() |>
    setNames(nm=colnames(dataset))
  CoI.report <- tibble::as_tibble(CoI.report.df)

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
#'   includes the following:
#'   - **column.name**: column name
#'   - **duplicate**: logical indicating if the column is duplicate. FYI: only duplicate
#'   columns are returned
#'   - **duplicate.colName**: duplicate columns
#'
#'  _**Not a publicly available function at this time.**_
#'
#' @param data `tibble` or `data.frame` of interest
#' @param data.md5s the calculated md5 hashes
#'
#' @return data.frame with the above information
#'
#' @examples
#' \dontrun{
#' library(tibble)
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
#' }
#'
#' @author Emilio Xavier Esposito \email{emilio.esposito@@gmail.com}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
find.duplicate.cols <- function(data, data.md5s) {

  ## determine the pairwise duplicates ----
  pairwise.dup <- purrr::map2(data.md5s, data.md5s,
                              ~which(data.md5s == .x & data.md5s == .y))

  ## retain only columns with duplicates ----
  data.pairs <- pairwise.dup[sapply(pairwise.dup, length) > 1]

  dup.collection.n <- data.md5s[sapply(pairwise.dup, length) > 1] |>
    unique() |>
    length()

  data.pairs.summary.colNames <- sapply(data.pairs, names) |>
    colnames()


  ## construct the summary ----
  ## i am not really sure how it works, but the transpose |> as_tibble |> transpose |> as.data.frame
  ## results in a data.frame with the duplicate column names as a string in a
  ## single column
  if (dup.collection.n == 1) {
    data.pairs.summary <- sapply(data.pairs, names) |>
      tibble::as_tibble(.name_repair="minimal") |>
      # tibble::add_column("column.name"=data.pairs.summary.colNames, .before=1) |>
      pivot_longer(cols={{data.pairs.summary.colNames}}, names_to="column.name", values_to="match") |>
      arrange(column.name) |>
      group_by(column.name) |>
      nest(duplicate.colNames=match) |>
      ungroup() |>
      dplyr::mutate(duplicate.tf=TRUE)
  } else {
    data.pairs.summary <- sapply(data.pairs, names) |>
      t() |>
      tibble::as_tibble(.name_repair="minimal") |>
      t() |>
      as.data.frame() |>
      tibble::rownames_to_column(var="column.name") |>
      dplyr::mutate(duplicate.tf=TRUE) |>
      dplyr::rename("duplicate.colNames"="V1") |>
      dplyr::select(column.name, duplicate.tf, duplicate.colNames)
  }

  ## return results ----
  return(data.pairs.summary)

}


#' @title Dataset Metadata Summary
#'
#' @description When working with new datasets it is helpful to know what type
#'  of data (aka metadata) is included and easily retype the columns and provide new column
#'  names. This function identifies the type of data in each column and provides
#'  unique examples data within each column. The results are written to an Excel
#'  workbook. Adding new column names and types to the Excel workbook allows the
#'  _easy-ish_ ability to import the new column names into R, and assign them to the
#'  original dataset. The goal is to reduce the logistical burden of the user.
#'
#'  This function will likely evolve overtime.
#'   - **Oct/2024**: Changed name from `dataset.summary()` to `dataset.meta()`
#'   - **Oct/2024**: Removed the **new column names** and **re-defined column types**
#'     from the output
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
#' @importFrom dplyr row_number case_when select arrange
#'
#' @examples
#' \dontrun{
#' dataset.summary(dataset=ds.orig,
#'                 ExcelFileName="ds_Column-names-and-data-types-and-examples.xlsx",
#'                 n.examples=4)
#' }
#'
#' @author Emilio Xavier Esposito \email{emilio.esposito@@gmail.com}
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
  n.cols <- ncol(dataset)

  ## construct the md5 hashes ----
  dataset.md5s <- sapply(dataset, digest::digest)

  ## find duplicate columns ----
  ds.duplicate.cols <- find.duplicate.cols(data=dataset, data.md5s=dataset.md5s)

  ## determine column types ----
  ds.classes <- dplyr::reframe(dataset, across(everything(), ~base::class(.x)))
  if (nrow(ds.classes) == 1) {
    ds.classes <- tibble::add_row(ds.classes, ds.classes)
  }
  ds.colTypes <- tidyr::pivot_longer(ds.classes, cols=everything(), names_to="column.name", values_to="type") |>
    tibble::add_column(type.name=sort(rep_len(c("colType.1", "colType.2"), length.out=n.cols*2))) |>
    tidyr::pivot_wider(id_cols="column.name", values_from="type", names_from="type.name") |>
    dplyr::mutate(col.idx=row_number(),
                  colType.diff=case_when(colType.1!=colType.2~"CHECK",
                                         TRUE~NA),
                  md5.hash=dataset.md5s) |>
    select(column.name, col.idx,
           colType.1, colType.2, colType.diff, md5.hash)

  ## is blank? ----
  cell.blank.tf <- purrr::map_dfc(dataset, theHUB::is.BLANK)
  col.blank.n <- colSums(cell.blank.tf, na.rm=TRUE)

  ## is NA? ----
  cell.NA.tf <- purrr::map_dfc(dataset, is.na)
  col.NA.n <- colSums(cell.NA.tf, na.rm=TRUE)

  ## add columns indicating number of blanks and NAs ----
  ds.colTypes <- tibble::add_column(ds.colTypes,
                                    n.TOT=n.rows,
                                    n.BLANK=as.integer(col.NA.n),
                                    n.NA=as.integer(col.NA.n))

  ## extract examples ----
  ds.examples <- extract.unique(dataset=dataset,
                                cell.blank.tf=cell.blank.tf,
                                cell.NA.tf=cell.NA.tf,
                                size=n.examples) |>
    t() |>
    as.data.frame() |>
    tibble::rownames_to_column(var="column.name") |>
    tibble::as_tibble()

  ## create and apply example names ----
  example.names <- paste("example.", seq_len(length.out=n.examples), sep="")
  ds.examples <- setNames(object=ds.examples, nm=c("column.name", "n.unique", example.names)) |>
    mutate(n.unique=as.integer(n.unique))

  ## create dataset for export ----
  ds.summary <- dplyr::full_join(x=ds.colTypes, y=ds.examples, by=c("column.name")) |>
    dplyr::left_join(y=ds.duplicate.cols, by=c("column.name")) |>  ## add in duplicate column information
    select(column.name:n.unique, duplicate.tf, duplicate.colNames, starts_with("example"))

  ## sort on md5s ----
  if (group.same.cols == TRUE) {
    ds.summary <- arrange(ds.summary, md5.hash, col.idx)
  }

  ## write out to Excel ----
  WriteXLS::WriteXLS(x=ds.summary,
                     ExcelFileName=ExcelFileName,
                     FreezeRow=1, FreezeCol=11,
                     AutoFilter=TRUE, AdjWidth=TRUE, BoldHeaderRow=TRUE)

  ## return examples ----
  return(ds.summary)
}


#' @title Replace Column Names
#'
#' @description Replace original column names with new column names.
#'
#' @details The function was initially designed to convert Likert responses to
#'   integers, but it was quickly realized that it could easily be used for a
#'   multitude of sins against data. The function relies on a user provided
#'   `tibble` or `data.frame` with two columns; one with the characters to be
#'   converted and the characters to be converted to.
#'
#'   The function is designed to work with [dplyr::mutate()] allowing multiple
#'   conversions in a single command.
#'
#'   _**Note**_: Ideally, the integers are positive and non-zero.
#'
#' @param data `tibble` (or `data.frame`) to have the column names replaced.
#' @param conversion.tb `tibble` (or `data.frame`) with the matched convertee and
#'   converted pairs; see the example below.
#' @param original column in the `conversion.tb` containing the original column names to be converted
#' @param new column in the `conversion.tb` containing the new column names being converted to
#' @param new.cols.only logical indicating if _**ONLY**_ changed columns should be returned; default: `FALSE`
#' @param verbose logical indicating if all the column names _**NOT**_ replaced will be returned; default: `TRUE`
#'
#' @return a `tibble` (or `data.frame`) with column names replaced (based on user
#'   provided parameters).
#' @export
#'
#' @examples
#' set.seed(13)
#' phrase2int.tb <- tibble::tibble(phrase=c("hated it!", "meh", "loved it!"),
#'                                 integer=c(-1, 0, 1))
#' responses.words <- sample(x=c("hated it!", "meh", "loved it!"), size=5, replace=TRUE)
#' responses.integers <- sample(x=c(-1, 0, 1), size=5, replace=TRUE)
#'
#' convert.fromto(responses=responses.words,
#'                fromto.tb=phrase2int.tb,
#'                from="phrase", to="integer")
#' # [1]  1 -1  0 -1  0
#'
#' convert.fromto(responses=responses.integers,
#'                fromto.tb=phrase2int.tb,
#'                from="integer", to="phrase")
#' # [1] "meh"       "hated it!" "loved it!" "hated it!" "meh"
#'
#' \dontrun{
#' replace.colNames(tibble.oi, Q1.ints=convert.fromto(responses=Q1,
#'                                          fromto.tb=phrase2int.tb,
#'                                          from="phrase", to="integer"))
#' }
#'
#' @author Emilio Xavier Esposito \email{emilio.esposito@@gmail.com}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
replace.colNames <- function(data, conversion.tb, original, new, new.cols.only=FALSE, verbose=TRUE) {

  ## basic information ----
  data.nCol <- ncol(data)
  replacement.nCol <- nrow(conversion.tb)

  ## get all column names ----
  colNames.ORIG.tb <- tibble::tibble(data.orig=colnames(data)) |>
    mutate(index=row_number())
  conversion.original <- conversion.tb[[original]]
  conversion.new <- conversion.tb[[new]]
  conversion.tb.reFMT <- tibble::as_tibble(conversion.tb) |>
    select({{original}}, {{new}}) |>
    setNames(nm=c("original.colNames", "new.colNames"))
  # conversion.tb <- tibble::tibble(original=conversion.tb[[original]],
  #                                 new=conversion.tb[[new]])

  ## match columns ----
  # match.tb <- left_join(x=colNames.ORIG.tb, y=conversion.tb,
  match.tb <- left_join(x=colNames.ORIG.tb, y=conversion.tb.reFMT,
                        by=c("data.orig"="original.colNames"))
  new.colNames.tb <- filter(match.tb, !is.na(new.colNames))

  new.nCol <- nrow(new.colNames.tb)

  ## columns not in the data of interest ----
  missing.colNames.tb <- filter(match.tb, is.na(new.colNames))

  ## non-matching column names ----
  non.matching.colNames <- paste0(missing.colNames.tb$data.orig, collapse=", ")
  status.message <- paste0("The dataset of interest has ", data.nCol,
                           " columns and the replacement data has ", replacement.nCol,
                           " new column names. There were ", new.nCol,
                           " column names replaced.")
  message(status.message)

  if (verbose == TRUE) {
    notice.message <- "Here are the original column names that were NOT replaced."
    message(notice.message)
    print(paste0(missing.colNames.tb$data.orig, collapse=", "))
  }

  ## set column names ----
  if (new.cols.only == TRUE) {
    data <- setNames(object=data[, new.colNames.tb$index], nm=new.colNames.tb$new)
  } else {
    colnames(data)[new.colNames.tb$index] <- new.colNames.tb$new
  }

  ## return data ----
  return(data)
}

# slate.column.names -> conversion.tb
# set.seed(13)
# sample_n(slate.column.names, 25) -> conversion.tb
# replace.colNames(data=data, conversion.tb=conversion.tb, original="slate.original", new="slate.new", new.cols.only=TRUE, verbose=TRUE)


