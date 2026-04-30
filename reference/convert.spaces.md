# Convert all Spaces to "Normal" Spaces

Converts various type of spaces into "normal" spaces.

## Usage

``` r
convert.spaces(text)
```

## Arguments

- text:

  A string of words forming a sentence or phrase.

## Value

string of cleaned characters forming a sentence or phrase.

## Details

One of several text cleaning functions. The function replaces various
types of spaces with ASCII spaces that are breakable. The reason for
this function is to replace non-breakable space with standard, breakable
spaces.

## See also

Other "text cleaning":
[`clean.cntrl()`](https://emilioxavier.github.io/theHUB/reference/clean.cntrl.md),
[`extract.emails()`](https://emilioxavier.github.io/theHUB/reference/extract.emails.md)

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
if (FALSE) { # \dontrun{
  comment <- "All day I see class mates and send e-mails.   I miss East Lansing."

  convert.spaces(text=comment)
} # }
```
