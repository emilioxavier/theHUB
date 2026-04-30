# Construct Pseudo-Grades

Constructs pseudo-grades based on the assignment parameters constructed
by
[`make.assignment.params()`](https://emilioxavier.github.io/theHUB/reference/make.assignment.params.md).
Each row is a *unique student* with scores for requested number of
assignments. The pseudo-grades are assigned using the
[`stats::rbinom()`](https://rdrr.io/r/stats/Binomial.html) function.

## Usage

``` r
make.pseudo.grades(assignment.params)
```

## Arguments

- assignment.params:

  tibble of grade distributions constructed by
  [`make.assignment.params()`](https://emilioxavier.github.io/theHUB/reference/make.assignment.params.md).

## Value

`tibble` with the pseudo-grades

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
set.seed(13)
assignment.params <- make.assignment.params(n.students=5,
                                            n.assignments=5,
                                            max.points=25)
make.pseudo.grades(assignment.params)
#> # A tibble: 5 × 6
#>   student   assign.1 assign.2 assign.3 assign.4 assign.5
#>   <chr>        <int>    <int>    <int>    <int>    <int>
#> 1 student.1        0        0        0        0        0
#> 2 student.2        7        5        6        3       10
#> 3 student.3        7       13       14       15        8
#> 4 student.4       18       16       16       18       18
#> 5 student.5       25       25       25       25       25
# # A tibble: 5 × 6
#   student   assign.1 assign.2 assign.3 assign.4 assign.5
#  <chr>        <int>    <int>    <int>    <int>    <int>
# 1 student.1        0        0        0        0        0
# 2 student.2        7        5        6        3       10
# 3 student.3        7       13       14       15        8
# 4 student.4       18       16       16       18       18
# 5 student.5       25       25       25       25       25
```
