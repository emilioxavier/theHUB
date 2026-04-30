# Honours (or Honors) Course?

Determine if a course is an honours course based on the course code
(`crse_code`). The fourth character is an "H". This designation is based
on the ["Definitions of Course
Characteristics"](https://reg.msu.edu/Read/UCC/Courselistings.pdf)
document provided on the Office of the Registrar's website.

## Usage

``` r
is.honours(crse_code)
```

## Arguments

- crse_code:

  single character vector with the course code of interest.

## Value

logical

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
crse_code <- "183H"
is.honours(crse_code)
#> [1] TRUE
```
