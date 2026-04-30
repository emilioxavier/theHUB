# Construct Pairwise Heatmap Data

Construct the pairwise heatmap data for
[`likert.heatmap()`](https://emilioxavier.github.io/theHUB/reference/likert.heatmap.md).

This function is called by
[`likert.heatmap()`](https://emilioxavier.github.io/theHUB/reference/likert.heatmap.md)
but can be called individually.

## Usage

``` r
make.likert.heatmap.data(data, QoI, Qcompared, value.range)
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

`tibble` (or `data.frame`) containing the needed count, percentage, and
colour hex values to construct a heatmap with percentage values overlaid
on each tile of the heatmap.

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)
