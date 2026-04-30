# Get Main Version Number

Returns the main version number.

## Usage

``` r
clean.OSversion(Version.number)
```

## Arguments

- Version.number:

  A string with the version number; *e.g.*, "10.15.7". Because version
  numbers can have multiple points, they are not considered numbers
  within `R` and thus they are strings.

## Value

integer of the main version number

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
clean.OSversion("10.15.7")
#> [1] 10
# 10

clean.OSversion("11.3")
#> [1] 11
# 11
```
