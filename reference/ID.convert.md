# Convert Collection of Numerical IDs

Given a collection of numerical IDs, systematically change their values
by a user defined value. ***This method is inferior to the
[`ID.random()`](https://emilioxavier.github.io/theHUB/reference/ID.random.md)
method. No judgement.***

## Usage

``` r
ID.convert(ID, deID.value = 12345)
```

## Arguments

- ID:

  string with ID or collection of IDs

- deID.value:

  numeric value, preferably an integer, to add to the numeric ID;
  default: `12345`. Values are converted from string to numeric (real)
  back to string to avoid the `.Machine$integer.max` (`2,147,483,647`)
  problem.

## Value

string vector

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

Seth Walker <walker893@msu.edu> (<https://github.com/walker893>)

## Examples

``` r
IDs <- c("1234500000", "12345", NA, 12345, 1234.56, "puzzle", "00011111")
ID.convert(ID=IDs, deID.value=12345)
#> Warning: NAs introduced by coercion
#> [1] "1234512345" "24690"      NA           "24690"      "13579.56"  
#> [6] NA           "00023456"  
# [1] "1234512345" "24690"      NA           "24690"      "13579.56"   NA           "00023456"
# Warning message:
# In ID.convert(ID = IDs, deID.value = 12345) : NAs introduced by coercion
```
