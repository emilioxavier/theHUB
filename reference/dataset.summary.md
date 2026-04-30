# Dataset Metadata Summary

When working with new datasets it is helpful to know what type of data
(aka metadata) is included and easily retype the columns and provide new
column names. This function identifies the type of data in each column
and provides unique examples data within each column. The results are
written to an Excel workbook. Adding new column names and types to the
Excel workbook allows the *easy-ish* ability to import the new column
names into R, and assign them to the original dataset. The goal is to
reduce the logistical burden of the user.

This function will likely evolve overtime.

- **Oct/2024**: Changed name from `dataset.summary()` to
  `dataset.meta()`

- **Oct/2024**: Removed the **new column names** and **re-defined column
  types** from the output

***Re-running this command will overwrite previous versions of the
file!!***

## Usage

``` r
dataset.summary(
  dataset,
  ExcelFileName,
  n.examples = 4,
  overwriteXLS = FALSE,
  group.same.cols = TRUE
)
```

## Arguments

- dataset:

  `tibble` or `data.frame` of interest

- ExcelFileName:

  string indicating the Excel workbook filename. The value is passed to
  [`WriteXLS::WriteXLS()`](https://rdrr.io/pkg/WriteXLS/man/WriteXLS.html).
  ***Re-running this command will overwrite previous versions of the
  file!!***

- n.examples:

  integer value indicating the number examples to return. Passed to
  `size` of
  [`extract.unique()`](https://emilioxavier.github.io/theHUB/reference/extract.unique.md).

- overwriteXLS:

  logical to overwrite existing Excel workbook; default is `FALSE`

- group.same.cols:

  logical indicating if the columns should be grouped by those with the
  same information.

## Value

tibble of column names, types, and examples

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
if (FALSE) { # \dontrun{
dataset.summary(dataset=ds.orig,
                ExcelFileName="ds_Column-names-and-data-types-and-examples.xlsx",
                n.examples=4)
} # }
```
