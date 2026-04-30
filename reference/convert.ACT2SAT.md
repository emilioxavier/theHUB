# Convert ACT Score to SAT Score

Convert a user provided ACT score to the corresponding SAT score. This
function uses the [Princeton
Review](https://www.princetonreview.com/college-advice/act-to-sat-conversion)
conversion table. Because a range of SAT values equal a single ACT
score, the mean of the SAT scores provided in [The Princeton Review's
ACT to SAT Score Conversion
Chart](https://www.princetonreview.com/college-advice/act-to-sat-conversion)
is used for this conversion. Please see the the
[ACT.2.SAT](https://emilioxavier.github.io/theHUB/reference/ACT.2.SAT.md)
dataset for details.

## Usage

``` r
convert.ACT2SAT(ACT.score)
```

## Arguments

- ACT.score:

  ACT score as a number (float or integer). `NA`s and text (aka
  [`as.character()`](https://rdrr.io/r/base/character.html)) are
  converted to integer values via
  [`as.integer()`](https://rdrr.io/r/base/integer.html).

## Value

integer of the corresponding SAT score

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
ACT.score <- c(25, "34", NA, 25.25, "NA", 50)
convert.ACT2SAT(ACT.score)
#> [1] 1220 1540   NA 1220   NA   NA
# [1] 1215 1535   NA 1215   NA   NA
```
