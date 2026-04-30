# Extract Unique Values from Each Column

Given a `tibble` or `data.frame` extract unique examples for each
column. This function is not publically available, but maybe a future
version will be.

## Usage

``` r
extract.unique(dataset, cell.blank.tf, cell.NA.tf, size = 3)
```

## Arguments

- dataset:

  `tibble` or `data.frame` of interest

- cell.blank.tf:

  constructed within
  [`dataset.summary()`](https://emilioxavier.github.io/theHUB/reference/dataset.summary.md)
  using the command `purrr::map_dfc(dataset, is.BLANK)` and returns a
  tibble the same dimensions as `dataset` and indicates whether or not
  each cell is blank. `TRUE` indicates the cell is blank and `FALSE`
  indicates it contains some type of value. `NA`s are
  [`dplyr::coalesce()`](https://dplyr.tidyverse.org/reference/coalesce.html)d
  into `FALSE`.

- cell.NA.tf:

  constructed within
  [`dataset.summary()`](https://emilioxavier.github.io/theHUB/reference/dataset.summary.md)
  using the command `purrr::map_dfc(dataset, is.na)` and returns a
  tibble the same dimensions as `dataset` and indicates whether or not
  each cell is blank. `TRUE` indicates the cell contains a `NA` and
  `FALSE` indicates it contains some type of value.

- size:

  integer value indicating the number examples to return

## Value

tibble of examples

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)
