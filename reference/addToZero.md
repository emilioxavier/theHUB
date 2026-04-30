# Add X to Zero Values

Sometimes you need to increase a zero value (0) to prevent `-Inf` and
`Inf` values due to an accidental divide by zero (0).

## Usage

``` r
addToZero(x, amount = 1L)
```

## Arguments

- x:

  A numerical value; `NA`s and `Inf`s – including `-Inf`s.

- amount:

  numerical value (doubles, floats, and integers) to add to the zero (0)
  values.

## Value

numerical value

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
addToZero(x=0, amount=1L)
#> [1] 1
# [1] 1
addToZero(x=0, amount=0.001)
#> [1] 0.001
# [1] 0.001

```
