# Construct Likert Stacked, Barplot Data

Construct the Likert data for
[`likert.barplot.stacked()`](https://emilioxavier.github.io/theHUB/reference/likert.barplot.stacked.md).

## Usage

``` r
make.likert.barplot.data(data, likert2int.tb, from = "long", to = "short")
```

## Arguments

- data:

  `tibble` (or `data.frame`) with Likert data converted to integers. Use
  the
  [`convert.fromto()`](https://emilioxavier.github.io/theHUB/reference/convert.fromto.md)
  function to convert text responses to integers. ***Note***: Ensure the
  integers are positive and non-zero.

- likert2int.tb:

  `tibble` (or `data.frame`) with the conversion between the ***long***
  description and the ***integer*** value. See
  [`convert.fromto()`](https://emilioxavier.github.io/theHUB/reference/convert.fromto.md)
  for an example of the `tibble`'s construction.

- from:

  column in the `likert2int.tb` containing the characters to be
  converted

- to:

  column in the `likert2int.tb` containing the characters being
  converted to

## Value

`tibble` (or `data.frame`) containing the needed count, percentage, and
xxxx to construct a Likert stacked, bar chart with
[`likert.barplot.stacked()`](https://emilioxavier.github.io/theHUB/reference/likert.barplot.stacked.md).

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)
