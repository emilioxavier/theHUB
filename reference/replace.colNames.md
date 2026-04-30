# Replace Column Names

Replace original column names with new column names.

## Usage

``` r
replace.colNames(
  data,
  conversion.tb,
  original,
  new,
  new.cols.only = FALSE,
  verbose = TRUE
)
```

## Arguments

- data:

  `tibble` (or `data.frame`) to have the column names replaced.

- conversion.tb:

  `tibble` (or `data.frame`) with the matched convertee and converted
  pairs; see the example below.

- original:

  column in the `conversion.tb` containing the original column names to
  be converted

- new:

  column in the `conversion.tb` containing the new column names being
  converted to

- new.cols.only:

  logical indicating if ***ONLY*** changed columns should be returned;
  default: `FALSE`

- verbose:

  logical indicating if all the column names ***NOT*** replaced will be
  returned; default: `TRUE`

## Value

a `tibble` (or `data.frame`) with column names replaced (based on user
provided parameters).

## Details

The function was initially designed to convert Likert responses to
integers, but it was quickly realized that it could easily be used for a
multitude of sins against data. The function relies on a user provided
`tibble` or `data.frame` with two columns; one with the characters to be
converted and the characters to be converted to.

The function is designed to work with
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
allowing multiple conversions in a single command.

***Note***: Ideally, the integers are positive and non-zero.

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
set.seed(13)
phrase2int.tb <- tibble::tibble(phrase=c("hated it!", "meh", "loved it!"),
                                integer=c(-1, 0, 1))
responses.words <- sample(x=c("hated it!", "meh", "loved it!"), size=5, replace=TRUE)
responses.integers <- sample(x=c(-1, 0, 1), size=5, replace=TRUE)

convert.fromto(responses=responses.words,
               fromto.tb=phrase2int.tb,
               from="phrase", to="integer")
#> [1]  1 -1  0 -1  0
# [1]  1 -1  0 -1  0

convert.fromto(responses=responses.integers,
               fromto.tb=phrase2int.tb,
               from="integer", to="phrase")
#> [1] "meh"       "hated it!" "loved it!" "hated it!" "meh"      
# [1] "meh"       "hated it!" "loved it!" "hated it!" "meh"

if (FALSE) { # \dontrun{
replace.colNames(tibble.oi, Q1.ints=convert.fromto(responses=Q1,
                                         fromto.tb=phrase2int.tb,
                                         from="phrase", to="integer"))
} # }
```
