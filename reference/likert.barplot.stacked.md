# Likert (and Likert-like) Stacked Barplot

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
likert.barplot.stacked(
  data,
  colour.palette = msu.palette,
  legend.position = "bottom",
  label.col = "Label.long",
  label.colour = "black",
  label.font = "Helvetica",
  label.size = 4,
  display.minimum = 3
)
```

## Arguments

- data:

  `tibble` (or `data.frame`) with Likert data converted to integers. Use
  the
  [`convert.fromto()`](https://emilioxavier.github.io/theHUB/reference/convert.fromto.md)
  function to convert text responses to integers. ***Note***: Ensure the
  integers are positive and non-zero.

- colour.palette:

  the colour palette of interest; default: `"msu.palette"`

- legend.position:

  to legend's position; default: `"bottom"`

- label.col:

  string with the column containing the labels. Setting to `NULL` will
  result in no labels.

- label.colour:

  string with label colour. Can accept colour name (`"black"`) or hex
  code (`"#000000"`); default: `"black"`

- label.font:

  string with font family; default: `"Helvetica"`

- label.size:

  label sizes; default: `4`

- display.minimum:

  numerical value indicating the small number of responses to display a
  label; default: `3`. Thus, responses with 2 or less responses are
  displayed within the stacked barplot but are not labeled.

## Value

ggplot2 graphics object

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
if (FALSE) { # \dontrun{
likert2int.tb <- tibble::tibble(phrase=c("hated it!", "meh", "loved it!"),
                                integer=c(-1, 0, 1))
make.likert.barplot.data(data, likert2int.tb)
likert.barplot.stacked(data,
                       colour.palette=msu.palette,
                       legend.position="bottom",
                       display.minimum=3)
} # }
```
