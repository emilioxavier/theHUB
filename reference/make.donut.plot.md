# Construct Donut Plot

Construct the donut plot using the donut data constructed with
[`make.donut.data()`](https://emilioxavier.github.io/theHUB/reference/make.donut.data.md)
function. This function automatically determines if the provided data
includes information for creating a facet of donut plots and including
layers.

## Usage

``` r
make.donut.plot(
  donut.DATA,
  colour.palette,
  colour.outline = "grey90",
  label.col = NULL,
  label.size = 3.5,
  facet.nrow = NULL,
  facet.ncol = NULL
)
```

## Arguments

- donut.DATA:

  `tibble` (or `data.frame`) constructed via
  [`make.donut.data()`](https://emilioxavier.github.io/theHUB/reference/make.donut.data.md)

- colour.palette:

  vector of strings containing the colour palette.

- colour.outline:

  string indicating the colour of the donut's outline.

- label.col:

  string with the column containing the labels. Setting to `NULL` will
  result in no labels.

- label.size:

  label sizes; default: `3.5`

- facet.nrow:

  numeric
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  rows; default: `NULL` Only provide `facet.nrow` ***OR*** `facet.ncol`.

- facet.ncol:

  numeric
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  columns; default: `NULL` Only provide `facet.nrow` ***OR***
  `facet.ncol`.

## Value

ggplot2 graphic object

## Details

**Custom Colour Palette** When creating a collection of donut plots in a
facet it is important to have the same colours assigned to the same
segments for all donut plots. Often, each donut plot does not always
include the same collection of segments. The following code ensures that
the same segment-colour combination is used for each donut plot.

    custom.palette <- msu.palette[1:9]
    names(custom.palette) <- all.donut.DATA$Categories |> unique()

If a "named colour palette" is not provided, one is created from the
categories within the `donut.DATA` data.frame and the provided
`colour.palette` to ensure a consistent colour palette is used for all
donut plots.

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
if (FALSE) { # \dontrun{
  donut.DATA <- make.donut.data(data, category, facetBy=NULL,
                                category.order="count", levels.rev=FALSE,
                                r.inner=4, r.outer=6)
  donut.PLOT <- make.donut.plot(donut.DATA, colour.palette=msu.palette,
                                label.col=NULL, label.size=3.5,
                                facet.nrow=NULL, facet.ncol=NULL)
} # }
```
