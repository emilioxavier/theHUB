# Country and Currency Dataset

The 257 International Organization for Standardization (ISO) recognized
countries and the currencies they use.

## Usage

``` r
country.currency
```

## Format

A tibble of countries and their associated currency.

## Source

Data sources:

- List of countries (via `ISOcodes::ISO_3166_1`) from ISOcodes
  ([CRAN](https://cran.r-project.org/web/packages/ISOcodes/))

- List of currencies (via `priceR::currencies()`) from priceR
  ([CRAN](https://cran.r-project.org/package=priceR) &
  [GitHub](https://github.com/stevecondylios/priceR))

- List of countries (via
  [`ggplot2::map_data()`](https://ggplot2.tidyverse.org/reference/map_data.html))

- List of countries (via [GADM maps and data version
  3.6](https://gadm.org/index.html))

- Wikipedia's [List of circulating
  currencies](https://en.wikipedia.org/wiki/List_of_circulating_currencies)

- Individual currency [Wikipedia](https://en.wikipedia.org/) pages (too
  many to list)

- Individual country [Wikipedia](https://en.wikipedia.org/) pages (also,
  too many to list)

## See also

Other Datasets:
[`ACT.2.SAT`](https://emilioxavier.github.io/theHUB/reference/ACT.2.SAT.md),
[`MSUpeers`](https://emilioxavier.github.io/theHUB/reference/MSUpeers.md)

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)
