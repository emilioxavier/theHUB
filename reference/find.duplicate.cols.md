# Find Duplicate Columns

Identifies and returns duplicate columns. The resulting data.frame
includes the following:

- **column.name**: column name

- **duplicate**: logical indicating if the column is duplicate. FYI:
  only duplicate columns are returned

- **duplicate.colName**: duplicate columns

***Not a publicly available function at this time.***

## Usage

``` r
find.duplicate.cols(data, data.md5s)
```

## Arguments

- data:

  `tibble` or `data.frame` of interest

- data.md5s:

  the calculated md5 hashes

## Value

data.frame with the above information

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
if (FALSE) { # \dontrun{
library(tibble)
data <- tibble(first.name=c("Alice", "Bob", "Carl", "Debbie"),
                  last.name=c("Masters", "Roberts", "Roberts", "Smith"),
                  pref.name=c("Alice", "Bob", "Carl", "Debbie"),
                  role=c("data", "data", "coding", "data"),
                  job=c("data", "data", "coding", "data"),
                  expert=c("data", "data", "coding", "data"),
                  building=c("Chemistry", "Biochemistry", "Chemistry", "Statistics"))
find.duplicate.cols(data=data)
#  column.name duplicate.tf    duplicate.colNames
#1  first.name         TRUE first.name, pref.name
#2   pref.name         TRUE first.name, pref.name
#3        role         TRUE     role, job, expert
#4         job         TRUE     role, job, expert
#5      expert         TRUE     role, job, expert
} # }
```
