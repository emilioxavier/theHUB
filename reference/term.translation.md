# Term-Code Translation Table

Term-code translation table used to convert four-digit term-code into
term and four-digit year. The `term.translation` constant contains the
following information. Used in the function
[`convert.termCode()`](https://emilioxavier.github.io/theHUB/reference/convert.termCode.md).

|                       |                          |                     |
|-----------------------|--------------------------|---------------------|
| **Term abbreviation** | **Full Term Name**       | **Short Term Name** |
| 1                     | Winter Quarter (WinterQ) | WQ                  |
| 2                     | Spring                   | SS                  |
| 3                     | Spring Quarter (SpringQ) | SpQ                 |
| 5                     | Summer                   | SU                  |
| 6                     | Summer Quarter           | SuQ                 |
| 8                     | Fall                     | FS                  |
| 9                     | Fall Quarter (FallQ)     | FQ                  |

## Usage

``` r
term.translation
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 7
rows and 3 columns.

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)
