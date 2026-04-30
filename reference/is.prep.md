# Course Prep?

Determine if a course is a preparation, aka prep, course based on the
course code (`crse_code`). The fourth character, in this case a digit is
five (5) or less. This designation is based on the ["Definitions of
Course
Characteristics"](https://reg.msu.edu/Read/UCC/Courselistings.pdf)
document provided on the Office of the Registrar's website.

## Usage

``` r
is.prep(crse_code)
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
crse_code <- "1831"
is.prep(crse_code)
#> [1] TRUE
# TRUE

crse_code <- "1838"
is.prep(crse_code)
#> [1] FALSE
# FALSE
```
