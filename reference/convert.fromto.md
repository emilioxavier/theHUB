# Phrase/Integer Conversion

Convert a collection of characters to another set of characters.

## Usage

``` r
convert.fromto(responses, fromto.tb, from, to, ignore.case = FALSE)
```

## Arguments

- responses:

  column to be converted; *e.g.*, `Q1`

- fromto.tb:

  `tibble` (or `data.frame`) with the matched convertee (`from`) and
  converted (`to`) pairs; see the examples below.

- from:

  column in the `fromto.tb` containing the characters to be converted

- to:

  column in the `fromto.tb` containing the characters being converted to

- ignore.case:

  logical indicating if the case of the `responses` and the `from`
  column of the `fromto.tb` should be ignored; default: `FALSE`

## Value

a vector of converted characters/phrases

## Details

The function was initially designed to convert Likert responses to
integers, but it was quickly realized that it could easily be used for a
multitude of sins against data. The function relies on a user provided
`tibble` or `data.frame` with two columns; one with the characters to be
converted and the characters to be converted to.

The function is designed to work with
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
allowing multiple conversions in a single command.

***Note***: Ideally, the integers are positive and non-zero.

When converting from text (aka character data) to numerical values, the
resulting numerical values are returned as integers or numerical data.

When values within the `responses` are not present within the
`fromto.tb` the original value(s) is returned to the user. If the `from`
values are characters and the `to` values are numeric, the `response`
values not present in the `from` column of the `fromto.tb` are replaced
with an `NA`. ***NB***: Don't worry, an `warning` happens to let you
know that a character string was converted to an `NA` via the standard
`NAs introduced by coercion`.

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
set.seed(13)
phrase2int.tb <- tibble::tibble(phrase=c("hated it!", "meh", "loved it!"),
                                integer=c(-1, 0, 1))
responses.words <- sample(x=c("hated it!", "meh", "loved it!"), size=5, replace=TRUE)
responses.integers <- sample(x=c(-1, 0, 1), size=5, replace=TRUE)

convert.fromto(responses=responses.words,
               fromto.tb=phrase2int.tb,
               from="phrase", to="integer")
#> [1]  1 -1  0 -1  0
# [1]  1 -1  0 -1  0

convert.fromto(responses=c(responses.words, "ugh"),
               fromto.tb=phrase2int.tb,
               from="phrase", to="integer")
#> Warning: NAs introduced by coercion
#> [1]  1 -1  0 -1  0 NA
# [1]  1 -1  0 -1  0 NA

convert.fromto(responses=responses.integers,
               fromto.tb=phrase2int.tb,
               from="integer", to="phrase")
#> [1] "meh"       "hated it!" "loved it!" "hated it!" "meh"      
# [1] "meh"       "hated it!" "loved it!" "hated it!" "meh"

YesNo2YN.tb <- tibble::tibble(long=c("Yes", "No"), short=c("Y", "N"))
responses.YesNo.1 <- c("Yes", "Yes", "Yes", "Yes", "No", "Yes", "No", "No", "No", "No")
responses.YesNo.2 <- c("Yes", "Yes", "Yes", "Yes", "No", "Yes", "No", "No", "No", "No", "Y", "N", "M", "yes")

convert.fromto(responses=responses.YesNo.1,
               fromto.tb=YesNo2YN.tb,
               from="long", to="short")
#>  [1] "Y" "Y" "Y" "Y" "N" "Y" "N" "N" "N" "N"
# [1] "Y" "Y" "Y" "Y" "N" "Y" "N" "N" "N" "N"

convert.fromto(responses=responses.YesNo.2,
               fromto.tb=YesNo2YN.tb,
               from="long", to="short",
               ignore.case=FALSE)
#>  [1] "Y"   "Y"   "Y"   "Y"   "N"   "Y"   "N"   "N"   "N"   "N"   "Y"   "N"  
#> [13] "M"   "yes"
# [1] "Y"   "Y"   "Y"   "Y"   "N"   "Y"   "N"   "N"   "N"   "N"   "Y"   "N"   "M"   "yes"

convert.fromto(responses=responses.YesNo.2,
               fromto.tb=YesNo2YN.tb,
               from="long", to="short",
               ignore.case=TRUE)
#>  [1] "Y" "Y" "Y" "Y" "N" "Y" "N" "N" "N" "N" "Y" "N" "M" "Y"
# [1] "Y" "Y" "Y" "Y" "N" "Y" "N" "N" "N" "N" "Y" "N" "M" "Y"

name2abbrev.tb <- tibble::tibble(name=c("Maine", "Michigan", "Ohio"), abbrev=c("ME", "MI", "OH"))
responses.states.1 <- c("MI", NA, "Michigan", NA, "Ohio", "PA", "ME", "Maine", "WA", "Pennsylvania")
responses.states.2 <- c("MI", "Mi", NA, "Michigan", NA, "Ohio", "ohio", "PA", "ME", "Maine", "WA", "Pennsylvania")
convert.fromto(responses=responses.states.1,
               fromto.tb=name2abbrev.tb,
               from="name", to="abbrev",
               ignore.case=FALSE)
#>  [1] "MI"           NA             "MI"           NA             "OH"          
#>  [6] "PA"           "ME"           "ME"           "WA"           "Pennsylvania"
# [1] "MI"           NA             "MI"           NA             "OH"           "PA"           "ME"           "ME"           "WA"           "Pennsylvania"

convert.fromto(responses=responses.states.1,
               fromto.tb=name2abbrev.tb,
               from="name", to="abbrev",
               ignore.case=TRUE)
#>  [1] "MI"           NA             "MI"           NA             "OH"          
#>  [6] "PA"           "ME"           "ME"           "WA"           "Pennsylvania"
# [1] "MI"           NA             "MI"           NA             "OH"           "PA"           "ME"           "ME"           "WA"           "Pennsylvania"

convert.fromto(responses=responses.states.2,
               fromto.tb=name2abbrev.tb,
               from="name", to="abbrev",
               ignore.case=FALSE)
#>  [1] "MI"           "Mi"           NA             "MI"           NA            
#>  [6] "OH"           "ohio"         "PA"           "ME"           "ME"          
#> [11] "WA"           "Pennsylvania"
# [1] "MI"           "Mi"           NA             "MI"           NA             "OH"           "ohio"         "PA"           "ME"           "ME"           "WA"           "Pennsylvania"

convert.fromto(responses=responses.states.2,
               fromto.tb=name2abbrev.tb,
               from="name", to="abbrev",
               ignore.case=TRUE)
#>  [1] "MI"           "Mi"           NA             "MI"           NA            
#>  [6] "OH"           "OH"           "PA"           "ME"           "ME"          
#> [11] "WA"           "Pennsylvania"
# [1] "MI"           "Mi"           NA             "MI"           NA             "OH"           "OH"           "PA"           "ME"           "ME"           "WA"           "Pennsylvania"


if (FALSE) { # \dontrun{
mutate(tibble.oi, Q1.ints=convert.fromto(responses=Q1,
                                         fromto.tb=phrase2int.tb,
                                         from="phrase", to="integer"))
} # }
```
