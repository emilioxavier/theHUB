# Construct Unified Terms

In text analysis, the concept of compounds words is difficult to
accurately capture, especially when performing term frequency analysis.
To accommodate the identification and counting of a multi-term
idea/concept, terms comprised of two or more words are combined into a
unified term.

## Usage

``` r
unified.terms(comment, clean = TRUE)
```

## Arguments

- comment:

  string of characters (usually text) to be evaluated and unified.
  Alternatively, the column containing the comments can be passed to the
  function as often is done via
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)

- clean:

  logical indicating if the function should also clean the comments to
  be unified; default: `TRUE`. See
  [`clean.comment()`](https://emilioxavier.github.io/theHUB/reference/clean.comment.md)
  for the details on comment cleaning.

## Value

vector of comments where predetermined terms are unified (and possibly
cleaned unless the user has changed the default). ***NOTE***: Unified
terms are camel case. For example, "wi-fi" is returned as "WiFi" and
"burn out" is returned as "burnOut".

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
comment <- "The Wi-Fi is down. My mental health is bad because  I lost my financial Aid."

unified.terms(comment=comment)
#> [1] "the WiFi is down. my mentalHealth is bad because i lost my finAid."
# "the WiFi is down. my mentalHealth is bad because i lost my finAid."
```
