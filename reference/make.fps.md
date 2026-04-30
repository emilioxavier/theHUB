# Make Fingerprints

Construct fingerprints for a collection of samples from the provided
integer and float values.

## Usage

``` r
make.fps(data, id.col, cols.oi)
```

## Arguments

- data:

  data.frame or tibble containing raw data to convert into fingerprints

- id.col:

  string indicating the identifying column

- cols.oi:

  vector of integers or strings indicting the columns of interest

## Value

tibble of fingerprints with integer (count) or float values

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
if (FALSE) { # \dontrun{
  library(tidyverse)
  where2eat <- tibble::tibble(what=c("BBQ", "Burgers", "Pizza"),
                              loc=c(1,2,3),
                              size=c(4,2,6),
                              cost=c(10,9,8))
  make.fps(data=where2eat, id.col="what", cols.oi=c("loc", "size", "cost"))
} # }
```
