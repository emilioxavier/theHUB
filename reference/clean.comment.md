# Clean Comments

Normalize (aka *clean*) comments to reduce the number of small counts
for similar or related words.

## Usage

``` r
clean.comment(comment)
```

## Arguments

- comment:

  string of characters (usually text) to be evaluated and unified.
  Alternatively, the column containing the comments can be passed to the
  function as often is done via
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)

## Value

vector of comments where predetermined terms are cleaned. ***NOTE***:
Some cleaned terms are returned in camel case. For example, "e-mail" is
returned as "eMail" and "face to face" and "face-to-face" are returned
as "faceTOface".

## Details

Often similar words or ideas have different forms depending on their
location within a sentence. For example, the term "advisor" can have
three different forms within a comment: "adviser", "advisers",
"advisors" and all three are converted (transformed) to "advisor".

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
comment <- "All day I see class mates and send e-mails.   I miss East Lansing."

clean.comment(comment=comment)
#> [1] "allDay i see classmate and send eMail. i instructor eastLansing."
# "allDay i see classmate and send eMail. i miss eastLansing."
```
