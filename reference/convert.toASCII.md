# Convert to ASCII

Converts non-local characters to ASCII

## Usage

``` r
convert.toASCII(comment)
```

## Arguments

- comment:

  A string of words forming a sentence or phrase.

## Value

string of cleaned characters forming words

## Details

Convert ***latin1*** encoded characters to ASCII

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
comment <- "All day I see class mates and send e-mails.   I miss East Lansing."

convert.toASCII(comment=comment)
#> [1] "All day I see class mates and send e-mails.   I miss East Lansing."
```
