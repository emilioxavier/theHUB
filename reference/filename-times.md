# File Name Date and Time

Construct date and time components for a file name. Ideally, this
function is called when the script (or main function) is initiated.

- `file.date()`: returns the date in "%b%d%Y" format; *e.g.*,
  "Jan232021"

- `file.time()`: returns the time in "%H%M" format; *e.g.*, "1453"

- `file.datetime()`: returns the date and time in the "%b%d%Y\_%H%M"
  format; *e.g.*, "Jan232021_1453"

## Usage

``` r
file.datetime(date.time = NULL)

file.date(date.time = NULL)

file.time(date.time = NULL)
```

## Arguments

- date.time:

  string extracted using
  [`base::Sys.time()`](https://rdrr.io/r/base/Sys.time.html) or passing
  `NULL` will result in the time the
  [`Sys.time()`](https://rdrr.io/r/base/Sys.time.html) function is
  called being used.

  ***NOTE***: Do ***NOT*** pass `NULL` to the individual date and time
  functions. Instead, call `file.datetime()` and split the string using
  [`base::strsplit()`](https://rdrr.io/r/base/strsplit.html) in the form
  `strsplit(x=file.datetime(), split="_")` to obtain the date and time.

## Value

string with the date, time, or date_time in the format noted above.

- For `file.date()`, the date "Jan232021"

- For `file.time()`, the time "1453"

- For `file.datetime()`, the date and time "Jan232021_1453"

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
date.time <- as.POSIXlt("2021-01-23 14:53:13 EST")

file.date(date.time=date.time)
#> [1] "Jan232021"
# "Jan232021"

file.time(date.time=date.time)
#> [1] "1453"
# "1453"

file.datetime(date.time=date.time)
#> [1] "Jan232021_1453"
# "Jan232021_1453"
```
