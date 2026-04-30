# Construct Keyword Similarity Plots

Construct the pairwise heatmap data for keyword area overlap

## Usage

``` r
keywords.plot.data(keywords.similarities)
```

## Arguments

- keywords.similarities:

  results of
  [`compare.dataset()`](https://emilioxavier.github.io/theHUB/reference/compare.dataset.md).

## Value

`tibble` with the data to construct a heatmap

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
if (FALSE) { # \dontrun{
keywords.comp.sim <- compare.dataset(data)

keywords.heatmap.data <- keywords.plot.data(keywords.similarities=keywords.comp.sim)
} # }
```
