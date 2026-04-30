# Construct Likert (and Likert-like) Pairwise Heatmap

Constructs a
[ggplot2::ggplot2](https://ggplot2.tidyverse.org/reference/ggplot2-package.html)
heatmap for Likert and Likert-like results with each option for the
question of interest (`QoI`) represented on the row while the columns
are the question being compared (`Qcompared`). Each tile (square) is
shaded based on the percent overlap and the percentage of overlapping
responses are noted as MSU dark-green text on a white box.

This function is called by
[`likert.plot.matrix()`](https://emilioxavier.github.io/theHUB/reference/likert.plot.matrix.md)
but can be called individually.

## Usage

``` r
likert.heatmap(data, QoI, Qcompared, value.range)
```

## Arguments

- data:

  `tibble` (or `data.frame`) with Likert data converted to integers. Use
  the
  [`convert.fromto()`](https://emilioxavier.github.io/theHUB/reference/convert.fromto.md)
  function to convert text responses to integers. ***Note***: Ensure the
  integers are positive and non-zero.

- QoI:

  string with the Question of Interest (the rows)

- Qcompared:

  string with the Question being Compared to (the columns)

- value.range:

  range of possible Likert values

## Value

ggplot2 graphics object

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
if (FALSE) { # \dontrun{
likert.heatmap(data=fs20.likert, QoI="Q1", Qcompared="Q2")
} # }
```
