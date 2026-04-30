# Time Since UNIX Epoch

Calculate time difference from UNIX epoch to current time

This function calculates the time difference between the UNIX epoch
(1970-01-01 00:00:00 UTC) and the current system time.

## Usage

``` r
origin2now(units = "mins")
```

## Arguments

- units:

  A character string specifying the time units for the result. Valid
  options are "auto", "secs", "mins", "hours", "days", "weeks". Default
  is "mins". Invalid units will be reset to "mins" with a warning.

## Value

A difftime object representing the time elapsed since the UNIX epoch in
the specified units.

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
# Get time since epoch in minutes (default)
origin2now()
#> Time difference of 29626260 mins

# Get time since epoch in hours
origin2now("hours")
#> Time difference of 493771 hours

# Get time since epoch in days
origin2now("days")
#> Time difference of 20573.79 days
```
