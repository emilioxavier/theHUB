# Is String Blanks?

Determine which strings within a vector of strings is either a string
with no characters or a string comprised entirely of blanks. This
function uses
[`stringr::str_detect()`](https://stringr.tidyverse.org/reference/str_detect.html)
and the regular expression `"^[[:space:]]*$"` to determine if the string
contains no characters or all blanks.

## Usage

``` r
is.BLANK(string)
```

## Arguments

- string:

  or vector of strings to evaluate.

## Value

logical

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
string <- c("", " ", "  ", "   ", " abc", "abc ", " abc ")
is.BLANK(string=string)
#> [1]  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE
# [1]  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE
```
