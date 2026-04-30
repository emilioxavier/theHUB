# Construct the Search String

Construct the search string (aka "pattern") based on provided search
terms. The
[`get.unique.terms()`](https://emilioxavier.github.io/theHUB/reference/get.unique.terms.md)
and
[`get.bigrams.oi()`](https://emilioxavier.github.io/theHUB/reference/get.bigrams.oi.md)
functions use this function. The function is a simple wrapper for the
[`paste()`](https://rdrr.io/r/base/paste.html) function that surrounds
each search term with `\\b` and separates multiple terms with pipes `|`.

## Usage

``` r
create.search.string(search.terms)
```

## Arguments

- search.terms:

  vector of strings to use as the query (aka "pattern") string.

## Value

string with the terms to search the text string of interest, *e.g.*,
`"\\bTERM1\\b|\\bTERM2\\b"`.

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
if (FALSE) { # \dontrun{
search.terms <- c("dog", "cat", "fish")
create.search.string(search.terms)
"\\bdog\\b|\\bcat\\b|\\bfish\\b"
} # }
```
