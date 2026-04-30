# Clean Date

This function expands
[`clean.DoB()`](https://emilioxavier.github.io/theHUB/reference/clean.DoB.md)
function and uses the
[`lubridate::parse_date_time()`](https://lubridate.tidyverse.org/reference/parse_date_time.html)
function to determine the provided date by trying

- Month - Day - Year

- Year - Month - Day

- Day - Month - Year

- Month - Year

- Year -Month

The function determines if the provided date contains month-day-year or
month- year information.

**This function is currently not available. Need to add a logical
parameter.** A date with a year five-years before the current year or
later is reduced by 100-years. For example, if the resulting year is
2020 (this description was written in 2022) the returned date has a year
of 1920.

## Usage

``` r
clean.DATE(dates)
```

## Arguments

- dates:

  string with date of birth-like data information.

## Value

date

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
clean.DATE("03301995")
#> [1] "1995-03-30 UTC"
# "1995-03-30"

clean.DATE("03/30/1995")
#> [1] "1995-03-30 UTC"
# "1995-03-30"

clean.DATE("03/1995")
#> [1] "1995-03-01 UTC"
# "1995-03-30"

clean.DATE("031995")
#> [1] "1995-03-01 UTC"
# "1995-03-30"
```
