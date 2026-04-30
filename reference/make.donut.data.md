# Construct Donut Plot Data

Construct the `data.frame` for the
[`make.donut.plot()`](https://emilioxavier.github.io/theHUB/reference/make.donut.plot.md)
function. The raw `data.frame` or a summarised `data.frame` is
acceptable and will be converted into the needed data for the
construction of a donut plot.

## Usage

``` r
make.donut.data(
  data,
  category,
  category.order = "count",
  category.count = NULL,
  facetBy = NULL,
  layerBy = NULL,
  layer.order = "descend",
  layer.alpha.min = 0.75,
  levels.rev = FALSE,
  r.inner = 4,
  r.outer = 6
)
```

## Arguments

- data:

  `tibble` (or `data.frame`) with the column of interest. ***NOTE***: Do
  **NOT** use counted data.

- category:

  string with column of interest containing the categories to comprise
  the donut (*aka* ring). Only provide **ONE** column name.

- category.order:

  string indicating if you want the data to be ordered by `"count"` in
  ***decreasing*** order or by `"category"` in alphabetical order;
  default: `"count"`.

- category.count:

  string with the column containing "counts" for each "category." This
  parameter is ***required*** when the count (or total) for each row was
  pre-calculated and allows for the creation of donut data when raw data
  is not available and one only has the summarised values.

- facetBy:

  string indicating the column to group data by; for when you want to
  **facet** your donut plots via
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html);
  see
  [`make.donut.plot()`](https://emilioxavier.github.io/theHUB/reference/make.donut.plot.md).

- layerBy:

  string indicating the column to group data by; for when you want to
  **add layers** your donut plots; see
  [`make.donut.plot()`](https://emilioxavier.github.io/theHUB/reference/make.donut.plot.md).

- layer.order:

  string indicating the order of the layers. There are four options:

  - `ascend` where the inner ring (donut) has the smallest value and the
    outer donut has the greatest value

  - `descend` where the inner donut as the largest value and the outer
    ring has the smallest value

  - `alphabetical` where the rings are ordered alphabetically starting
    from the inner ring

  - `user defined` where the user provides the order of the donuts
    starting from inner ring. Only layers included in the vector
    (*e.g.*, `c("4", "r", "f")`) are included in the resulting data.

  For `alphabetical`, `ascend`, and `descend` only the first ***two***
  characters are needed.

- layer.alpha.min:

  value indicating the minimum alpha value; default: `0.75`

- levels.rev:

  logical indicating if the order of the categories should be reversed.

- r.inner:

  numeric value defining the inner radius of the donut; default: `4`

- r.outer:

  numeric value defining the outer radius of the donut; default: `6`

## Value

`tibble` with the data to construct the donut plot.

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
if (FALSE) { # \dontrun{
  donut.DATA <- make.donut.data(data, category, facetBy=NULL,
                                category.order="count", levels.rev=FALSE,
                                r.inner=4, r.outer=6)
} # }
```
