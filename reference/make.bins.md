# Bin Values

Bin and name a collection of values.

## Usage

``` r
make.bins(x, bins, bin.names)
```

## Arguments

- x:

  vector of (likely) integer values representing age

- bins:

  vector of bins for use with
  [`base::findInterval()`](https://rdrr.io/r/base/findInterval.html)

- bin.names:

  vector of bin names. ***NEEDS*** to be the same length as `bins`

## Value

string with interval values

## Details

Commonly used to bin ages and survey responses that are continuous;
*e.g.*, `1, 3, 1, 2, 5, 4`. Values that are not bin-able return a
`"Not Provided`" classification.

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
set.seed(13)
age <- c(NA, sample(1:10, 20, replace=TRUE), NA)
age.bins <- c(0, 2, 4, 6, 8, 10)
age.bin.names <- c("0", "2-3", "4-5", "6-7", "8-9", "10")
make.bins(x=age, bins=age.bins, bin.names=age.bin.names)
#>  [1] "Not Provided" "8-9"          "2-3"          "4-5"          "10"          
#>  [6] "6-7"          "6-7"          "4-5"          "8-9"          "2-3"         
#> [11] "0"            "10"           "6-7"          "6-7"          "4-5"         
#> [16] "0"            "6-7"          "8-9"          "0"            "0"           
#> [21] "4-5"          "Not Provided"
```
