# Get Unique Terms

Using the provided search terms, identify the phrases containing the
search terms, and return all the related terms.

## Usage

``` r
get.unique.terms(string.oi, search.terms)
```

## Arguments

- string.oi:

  vector of strings to search.

- search.terms:

  vector of strings to use as the query (aka "pattern") string.

## Value

string with the terms related to the query

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
