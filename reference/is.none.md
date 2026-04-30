# No Comment Present

Indicates if the presented comment was intended to be a "none- comment"
by the author. Steps to determine if the user supplied comment is
considered a non-comment:

- convert string to lower case

- count the number of words

- count the number of characters

- if the string is a single word OR the number of characters is zero (0)

  - remove all punctuation

  - if the single word is: "", "na", "no", "none", "nothing" indicate
    the comment was intended to be a "non-comment"

## Usage

``` r
is.none(comment)
```

## Arguments

- comment:

  string with the comment

## Value

logical indicating if the comment is a common non-response

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
 is.none("today is awesome!")
#> [1] FALSE
 # FALSE

 is.none("NA")
#> [1] TRUE
 # TRUE

 is.none("N/A")
#> [1] TRUE
 # TRUE

 is.none("")
#> [1] TRUE
 # TRUE
```
