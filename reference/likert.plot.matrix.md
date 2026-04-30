# Likert (and Likert-Like) Pairwise Analysis Plot

Graphical representation of pairwise interactions between Likert and
Likert-like responses. Likert-like responses are survey questions where
the respondent can only select one response.

## Usage

``` r
likert.plot.matrix(data, value.range, title, questions, footnotes = NULL)
```

## Arguments

- data:

  `tibble` (or `data.frame`) of Likert and categorical data with
  responses in integer form.

- value.range:

  XXXXXXX

- title:

  string with the title for the plot; *e.g.*, "Likert Pairwise Analysis"

- questions:

  vector of strings with shortened versions of the questions used to
  label the plots; *e.g.*, c("Q1. Overall", "Q2. MSU Leadership
  Communication"). Use `\n` to denoted line breaks (aka carriage
  returns).

- footnotes:

  vector of strings to represent what the numeric (integer) values
  represent. Use `\n` to denoted line breaks (aka carriage returns).

## Value

[ggplot2::ggplot2](https://ggplot2.tidyverse.org/reference/ggplot2-package.html)
object constructed via
[cowplot::cowplot](https://wilkelab.org/cowplot/reference/cowplot-package.html)

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
if (FALSE) { # \dontrun{
title <- "Likert & Likert-like Comparison"
questions <- c("Q1. Do you like everything bagels?", "Q2. Do you drink coffee?")
footnotes <- c("Q1. 1='Not a lot', 2='Meh', 3='Very much';\nQ2. 1=Never, 2=Some times, 3='Always'")

likert.plot.matrix(data=select(fs20.data, Q1, Q2),
                   title=title,
                   questions=questions,
                   footnotes=footnote)
} # }
```
