# Classification Check

Identify when a new class in the list of **long** responses that need to
be converted to their **short**ened forms.

## Usage

``` r
classification.check(data, col.oi, ct, ct.col)
```

## Arguments

- data:

  `tibble` or `data.frame` of interest

- col.oi:

  string indicating the column with classes of interest

- ct:

  `tibble` or `data.frame` conversion table with a column with the
  information to be converted and the information to be converted to

- ct.col:

  string indicating the column to match to the classes in the `col.oi`

## Value

Indicator of missing classifications. See Examples.

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
if (FALSE) { # \dontrun{
 classification.check(data=data.oi, col.oi="Animal", ct=ToFrom.ct, ct.col="long")
 # No new classes in the dataset of interest.

 classification.check(data=data.oi, col.oi="Animal", ct=ToFrom.ct, ct.col="long")
 # There are unmatched values in the data of interest:
 #  -> Dogs
 #  -> Cats
} # }
```
