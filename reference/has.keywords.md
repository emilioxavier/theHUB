# Keywords Present

Using either the
[keywords](https://emilioxavier.github.io/theHUB/reference/keywords.md)
provided within
[theHUB](https://emilioxavier.github.io/theHUB/reference/theHUB-package.md)
or a `tibble` with a collection of user defined terms and areas. The
`tibble` needs to have each keyword in the *keyword* column and the
corresponding area in the *area* column.

## Usage

``` r
has.keywords(
  data,
  column.oi,
  keywords.tb,
  kw.group.col,
  kw.query.col,
  ignore.case = FALSE
)
```

## Arguments

- data:

  `tibble` (or `data.frame`) with the comments of interest

- column.oi:

  string indicating the comments of interest; *e.g.*, `"comments.clean"`
  or `"event.name"`

- keywords.tb:

  `tibble` with the overall group and the query

- kw.group.col:

  `keywords.tb` containing the the overall area

- kw.query.col:

  `keywords.tb` containing the queries

- ignore.case:

  `logical` if `FALSE`, the pattern matching is case sensitive and if
  `TRUE`, case is ignored during matching; default is `FALSE`

## Value

`tibble` (or `data.frame`) with the original data plus a column for each
provided area. Each *area* column has a `TRUE` or `FALSE` for each row
indicating if the comment had a *keyword* present in the comment.

## Details

Convert the
[keywords](https://emilioxavier.github.io/theHUB/reference/keywords.md)
to a `tibble` with a row for each *area* using the following command.

    keywords
     A tibble: 206 x 2
      keyword    area
      <chr>      <chr>
    1 adviser    acadSupport
    2 advising   acadSupport
    3 advisor    acadSupport
    4 assignment assessment

    keywords.tb <- dplyr::group_by(keywords, area) |>
      dplyr::summarise(query=paste(keyword, collapse="|"))

    keywords.tb
    # A tibble: 16 x 2
      area          query
      <chr>         <chr>
    1 acadSupport   adviser|advising|advisor
    2 assessment    assignment|...

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)
keywords.tb <- group_by(keywords, area) |>
    summarise(query=paste(keyword, collapse="|"))

fs20.comments <- has.keywords(data=fs20.comments,
                              comment.col="comments.clean",
                              keywords.tb=keywords.tb)
} # }
```
