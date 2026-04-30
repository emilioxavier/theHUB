# Determine the Course Type

Determine if a course is a preparation (aka prep), honours (H),
multi-diversity (I), national diversity (N), international and
multicultural diversity (D), or writing (W) course based on the course
code (`crse_code`). Theese designations are based on the ["Definitions
of Course
Characteristics"](https://reg.msu.edu/Read/UCC/Courselistings.pdf)
document provided on the Office of the Registrar's website.

## Usage

``` r
course.type(crse_code)
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
course.type(crse_code)
#> [1] "prep"
# "prep"

crse_code <- "1838"
is.prep(crse_code)
#> [1] FALSE
# ""
```
