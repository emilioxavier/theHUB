# Make Course-Section Designation

Constructs the course-section designation to match the course-section
description in the `crsesect_firstday`, `crsesect_qrtrterm`, and
`crsesect_endterm` columns/fields

## Usage

``` r
make.crsesect(course, sctn_code)
```

## Arguments

- course:

  string containing the course designation; a combination of `subj_code`
  and `crse_code`. For example, `"PKG 491"`.

- sctn_code:

  string (or integer) with the section code.

## Value

string with the subject code, course code, and section code to form a
course-section designation in the same format as the
`crsesect_firstday`, `crsesect_qrtrterm`, and `crsesect_endterm`
columns/fields.

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
course <- "PKG 491"
sctn_code <- "6"

make.crsesect(course, sctn_code)
#> [1] "PKG 491-  6"
# "PKG 491-006"
```
