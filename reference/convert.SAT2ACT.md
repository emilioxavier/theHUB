# Convert SAT Score to ACT Score

Convert a user provided SAT score to the corresponding ACT score. This
function uses the [Princeton
Review](https://www.princetonreview.com/college-advice/act-to-sat-conversion)
conversion table. Because a range of SAT values equal a single ACT
score, a range of SAT scores will return the same ACT score. Please see
[The Princeton Review's ACT to SAT Score Conversion
Chart](https://www.princetonreview.com/college-advice/act-to-sat-conversion)
or the
[SAT.2.ACT](https://emilioxavier.github.io/theHUB/reference/ACT.2.SAT.md)
dataset for details.

## Usage

``` r
convert.SAT2ACT(SAT.score)
```

## Arguments

- SAT.score:

  SAT score as a number (float or integer)

## Value

integer of the corresponding ACT score

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
SAT.score <- c(1326, "1444", NA, 1444.44, "NA", 3600)
convert.SAT2ACT(SAT.score)
#> [1] 28 31 NA 31 NA NA
# [1] 28 31 NA 31 NA NA
```
