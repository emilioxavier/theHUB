# Convert Control Characters to Spaces

Converts control characters into physical spaces.

## Usage

``` r
clean.cntrl(text)
```

## Arguments

- text:

  A string of words forming a sentence or phrase.

## Value

string of cleaned characters forming a sentence or phrase.

## Details

One of several text cleaning functions. The function replaces the
following control characters `\n`, `\r`, `\t`, `\v`, and `\f` (those
identified by the `[:cntrl:]` regular expression) with a series of three
(3) physical spaces. While multiple, concurrent and connected control
characters (`\n\r\t\v\f`) would be replace by three spaces, control
characters separated by any printable character (defined by
`[:print:]`), even spaces, are retained; see example.

## See also

Other "text cleaning":
[`convert.spaces()`](https://emilioxavier.github.io/theHUB/reference/convert.spaces.md),
[`extract.emails()`](https://emilioxavier.github.io/theHUB/reference/extract.emails.md)

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
comment <- "All day I see class mates and send e-mails.\n\r\t\v\fI miss East Lansing."
clean.cntrl(text=comment)
#> [1] "All day I see class mates and send e-mails.   I miss East Lansing."
# [1] "All day I see class mates and send e-mails.   I miss East Lansing."

comment <- "All day I see class mates and send e-mails.\n  \r  \t  \v\fI miss East Lansing."
clean.cntrl(text=comment)
#> [1] "All day I see class mates and send e-mails.                  I miss East Lansing."
# [1] "All day I see class mates and send e-mails.                  I miss East Lansing."
```
