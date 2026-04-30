# Convert Integer Fingerprints into Binary Representation

Convert integer fingerprints into binary (logical) representation.

## Usage

``` r
convert.fps2binary(data, id.col)
```

## Arguments

- data:

  data.frame or tibble with fingerprint values

- id.col:

  string indicating the identifying column

## Value

tibble of fingerprints with logical (binary) values

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
  make.fps(data=where2eat, id.col="what", cols.oi=c("loc", "size", "cost")) |>
    convert.fps2binary()
} # }
```
