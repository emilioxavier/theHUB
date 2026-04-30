# Clean Year of Birth

Clean Year of Birth

## Usage

``` r
clean.YoB(YoB)
```

## Arguments

- YoB:

  string with date of birth-like data information

## Value

integer with the determined year of birth

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
clean.YoB("1969")
#> [1] 1969
# 1969

clean.YoB("01/23/1969")
#> [1] 1969
# 1969

clean.YoB("01/23/69")
#> [1] 1969
# 1969

clean.YoB("23 January 1969")
#> [1] 1969
# 1969

clean.YoB("19690123")
#> [1] 1969
# 1969

clean.YoB("69")
#> [1] 1969
# 1969
```
