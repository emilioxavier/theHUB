# Extract Email Address(es) From String

Extract all email address(es) with a specific domain (*e.g.*, msu.edu,
gmail.com, hotmail.com, etc.)

## Usage

``` r
extract.emails(emails.string, domains = "msu.edu")
```

## Arguments

- emails.string:

  string with emails.

- domains:

  string with the domains; *e.g.*, "msu.edu".

## Value

string of emails

## See also

Other "text cleaning":
[`clean.cntrl()`](https://emilioxavier.github.io/theHUB/reference/clean.cntrl.md),
[`convert.spaces()`](https://emilioxavier.github.io/theHUB/reference/convert.spaces.md)

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
extract.emails(emails.string="emilio.dork@gmail.com 2.that.dork@egr.msu.edu
dork@egr.msu.edu 517565656325@162.123 thatDork@msu.edu", domains="msu.edu")
#> [[1]]
#> [1] "2.that.dork@egr.msu.edu" "dork@egr.msu.edu"       
#> [3] "thatdork@msu.edu"       
#> 
# [1] "2.that.dork@egr.msu.edu" "thatdork@msu.edu"
```
