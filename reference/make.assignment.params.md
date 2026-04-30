# Construct Assignment Parameters

add the description of making the data

## Usage

``` r
make.assignment.params(n.students, n.assignments, max.points)
```

## Arguments

- n.students:

  number of students in the course; default: 15

- n.assignments:

  number of assignments or assessments; default: 10

- max.points:

  maximum number of points for all assignments; default: 25

## Value

`tibble` with the input parameters for make.pseudo.grades

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
assignment.params <- make.assignment.params(n.students=5,
                                            n.assignments=5,
                                            max.points=25)
# # A tibble: 5 × 3
# n  size  prob
# <dbl> <dbl> <dbl>
# 1     5    25  0
# 2     5    25  0.25
# 3     5    25  0.5
# 4     5    25  0.75
# 5     5    25  1
```
