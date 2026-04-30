# Find n-grams with Keywords

When making network map/plot of connected terms, the need to remove
cluster of terms with three or two terms is often desired. This function
identifies all n-grams connected to the user provided keyword(s).

## Usage

``` r
get.bigrams.oi(data, col.oi, search.terms)
```

## Arguments

- data:

  `data.frame` or `tibble` with an n-gram to search.

- col.oi:

  string with the column name of to search.

- search.terms:

  string with the keyword(s) of interest. Can be a single word (*e.g.*,
  "cat") or multiple words (*e.g.*, `c("dog", "cat")`).

## Value

`data.frame` or `tibble` (depending on what is provided) with the n-gram
phrases connected to the provided `search.terms` (aka keyword(s)).

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
if (FALSE) { # \dontrun{
popular.DATA <- get.bigrams.oi(data=bigrams.n3,
                               col.oi="gram.2",
                               search.terms=c("development", "learning", "health", "research"))
} # }
```
