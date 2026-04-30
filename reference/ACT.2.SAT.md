# ACT Score Conversion Table

The conversion tables used to convert a user provided ACT or SAT score
to the corresponding SAT or ACT score, or *vice versa*. These conversion
tables are based on the [The Princeton Review's ACT to SAT Score
Conversion
Chart](https://www.princetonreview.com/college-advice/act-to-sat-conversion)
conversion table.

The full range of SAT scores are used when calculating the mean SAT
score that corresponds with the singular ACT score. The table provided
by The Princeton Review indicates the starting and several midpoint SAT
values for each ACT value but does *not* indicate the ending value in
the SAT range. It should be noted that starting and ending SAT values
depends on your perspective, but in this case, we consider the smallest
SAT value for a specific ACT score to be the starting value and the
greatest SAT value to be the ending value. If we take the range of SAT
scores for an ACT score of 35, The Princeton Review table provides four
values (1560, 1570, 1580, and 1590) with the maximum ACT value of 36
corresponding to 1600 for SAT. Simply taking the mean of the four values
is 1575 but if the entire range of SAT values (1560-1599) is considered,
the mean is 1579.5. Since you cannot have a non-integer value for an SAT
score, the mean SAT score is rounded up to 1580. The process of
calculating the mean SAT score for each ACT score from the entire range
of SAT scores and rounding up was done to construct the ACT-SAT
conversion table.

## Usage

``` r
ACT.2.SAT

SAT.2.ACT

SAT.ACT.PR
```

## Format

An object of class `grouped_df` (inherits from `tbl_df`, `tbl`,
`data.frame`) with 26 rows and 2 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with
1041 rows and 2 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 105
rows and 2 columns.

## Source

Data source: [The Princeton Review's ACT to SAT Score Conversion
Chart](https://www.princetonreview.com/college-advice/act-to-sat-conversion)

## Functions

- `SAT.2.ACT`: SAT Score Conversion Table

- `SAT.ACT.PR`: The [original Princeton Review Conversation
  Table](https://www.princetonreview.com/college-advice/act-to-sat-conversion)

## See also

Other Datasets:
[`MSUpeers`](https://emilioxavier.github.io/theHUB/reference/MSUpeers.md),
[`country.currency`](https://emilioxavier.github.io/theHUB/reference/country.currency.md)

Other Datasets:
[`MSUpeers`](https://emilioxavier.github.io/theHUB/reference/MSUpeers.md),
[`country.currency`](https://emilioxavier.github.io/theHUB/reference/country.currency.md)

Other Datasets:
[`MSUpeers`](https://emilioxavier.github.io/theHUB/reference/MSUpeers.md),
[`country.currency`](https://emilioxavier.github.io/theHUB/reference/country.currency.md)

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)
