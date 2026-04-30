# Likert (and Likert-like) Barplot

Constructs a
[ggplot2::ggplot2](https://ggplot2.tidyverse.org/reference/ggplot2-package.html)
barplot for Likert and Likert-like results with each option displayed as
an individual, horizontal MSU dark-green bars. The percentage of
responses are noted on the bar as white text.

This function is called by
[`likert.plot.matrix()`](https://emilioxavier.github.io/theHUB/reference/likert.plot.matrix.md)
but can be called individually.

## Usage

``` r
likert.barplot(data, QoI, value.range)
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
likert.barplot(data=fs20.likert, QoI="Q1")
} # }
```
