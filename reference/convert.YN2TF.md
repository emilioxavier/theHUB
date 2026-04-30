# Convert Yes-No indicators to TRUE-FALSE

Often True-False data is returned as a vector of Ys and Ns or 1s and 0s.
These vectors often contain blanks or `NA`s that cannot, and should not,
be converted into Falses. This function converts

- "Y" –\>\> `TRUE`

- "N" –\>\> `FALSE`

- "Yes" –\>\> `TRUE`

- "No" –\>\> `FALSE`

- "1" –\>\> `TRUE`

- "0" –\>\> `FALSE`

- 1 –\>\> `TRUE`

- 0 –\>\> `FALSE`

and leave blanks and `NA`s in the vector.

## Usage

``` r
convert.YN2TF(YN.string)
```

## Arguments

- YN.string:

  string with "Yes", "No" indicators such as "Y", "N", "yes", "no", "1",
  or "0"

## Value

logical vector

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
YN.string <- c("Y", "N", "1", "0", "yes", "no", NA, NA)
# [1]  TRUE FALSE  TRUE FALSE    NA    NA
```
