# Convert Four-Digit Term Codes

Converts four-digit term codes to semester or quarter names with
four-digit years.

## Usage

``` r
convert.termCode(term.code, term.type = "full")
```

## Arguments

- term.code:

  Four-digit code indicating the semester and year of the term.

- term.type:

  Indicate if the full or short semester (or quarter) designation is
  returned; default: `"full"`.

## Value

string term and four-digit year

## Details

Within CampusSolutions the term code is a four digit representation of
the century (position 1), the two-digit calendar year (positions 2 and
3), and the term (position 4). The following is the translation between
digits and four-digit years and term types.

|              |                            |
|--------------|----------------------------|
| **Position** | **Information**            |
| 1            | Century Code               |
|              | (1 = 19xx & 2 = 20xx)      |
| 2 & 3        | Two-Digit Calendar Year    |
| 4            | Term Indicator (see below) |

The information in the following table is available in the constant
[term.translation](https://emilioxavier.github.io/theHUB/reference/term.translation.md).

|                       |                          |                     |
|-----------------------|--------------------------|---------------------|
| **Term abbreviation** | **Full Term Name**       | **Short Term Name** |
| 1                     | Winter Quarter (WinterQ) | WQ                  |
| 2                     | Spring                   | SS                  |
| 3                     | Spring Quarter (SpringQ) | SQ                  |
| 5                     | Summer                   | US                  |
| 6                     | Summer Quarter           | UQ                  |
| 8                     | Fall                     | FS                  |
| 9                     | Fall Quarter (FallQ)     | FQ                  |

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
term.code <- "2208"

convert.termCode(term.code, term.type="full")
#> [1] "Fall 2020"
# [1] "Fall 2020"

convert.termCode(term.code, term.type="short")
#> [1] "FS 2020"
# [1] "FS 2020"
```
