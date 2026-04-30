# Clean Date of Birth

This function uses the
[`lubridate::parse_date_time()`](https://lubridate.tidyverse.org/reference/parse_date_time.html)
function to determine the date of birth by trying

- Month - Day - Year

- Year - Month - Day

- Day - Month - Year

## Usage

``` r
clean.DoB(DoB)
```

## Arguments

- DoB:

  string with date of birth-like data information.

## Value

date with determined date of birth

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
clean.DoB("03301995")
#> [1] "1995-03-30"
# "1995-03-30"

clean.DoB("03/30/1995")
#> [1] "1995-03-30"
# "1995-03-30"
```
