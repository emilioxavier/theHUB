# Tanimoto Coefficient/Index

Calculate the Tanimoto coefficient/index (also known as the Jaccard
index). A value of `0` indicates **no similarity** and a value of `1`
indicates **perfect similarity**.

## Usage

``` r
Tanimoto.Idx(set1, set2)
```

## Arguments

- set1:

  vector of `TRUE`/`FALSE` values OR vector of integers. See **Details
  above**.

- set2:

  vector of `TRUE`/`FALSE` values OR vector of integers. See **Details
  above**.

## Value

numerical value indicating the Tanimoto coefficient

## Details

Either two `TRUE`/`FALSE` vectors or two `integer` vectors can be
provided. The `TRUE`/`FALSE` vectors ***must be the same lengths***
while the integer vectors only have to indicate indices of interest in
each vector; see examples.

The Tanimoto coefficient is an indication of how similar two samples are
to each other based on number of bits overlapping (in common) in
relationship to the number of unique bits present in both samples.

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
  set1 <- c(TRUE,TRUE,FALSE,FALSE,FALSE)
  set2 <- c(TRUE,FALSE,TRUE,TRUE,TRUE)
  Tanimoto.Idx(set1, set2)
#> [1] 0.2
  # 0.2

  set1 <- which(set1)
  set1
#> [1] 1 2
  # 1 2
  set2 <- which(set2)
  set2
#> [1] 1 3 4 5
  # 1 3 4 5
  Tanimoto.Idx(set1, set2)
#> [1] 0.2
  # 0.2
```
