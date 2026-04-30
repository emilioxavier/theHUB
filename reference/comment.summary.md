# Comment Summary

Brief, numeric summary of the comments added to the `tibble` (or
`data.frame`).

## Usage

``` r
comment.summary(data, comment.col)
```

## Arguments

- data:

  `tibble` (or `data.frame`) with the comments of interest

- comment.col:

  column name with the comments; *e.g.*, `"comments"`

## Value

augmented `tibble` (or `data.frame`) with the following additional
information:

- is the comment a non-comment (see
  [`is.none()`](https://emilioxavier.github.io/theHUB/reference/is.none.md))

- number of words in the comment (see
  [`word.count()`](https://emilioxavier.github.io/theHUB/reference/word.count.md))

- number of keyword areas contained within the comment

A summary for all comments is returned to the user.

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
if (FALSE) { # \dontrun{
comment.summary(data=comment.data, comment.col="comment.clean")
} # }
```
