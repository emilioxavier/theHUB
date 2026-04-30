# Compare Datasets

Calculates the amount of similarity (or dissimilarity based on how you
are looking at the data) within a dataset.

## Usage

``` r
compare.dataset(data)
```

## Arguments

- data:

  `tibble` (or `data.frame`) to be compared

## Value

three matrices are returned.

- **Tanimoto.Indices**: pairwise Tanimoto coefficients for all samples

- **Overlap.Counts**: number of overlapping samples for each area of
  interest. Must be read column-wise because the number of counts is for
  each row area within the column.

- **Overlap.Ratios**: ratio (aka fraction) of number of overlapping
  samples for each area of interest. Must be read column-wise because
  the ratio is for each row area within the column.

## Details

The number and ratio (aka fraction) of overlapping samples within each
area of interest along with the Tanimoto coefficient/index are
calculated. The returned Tanimoto coefficients/indice matrices are
symmetrical around the diagonal, but the count and ratio matrices are
not. ***They can be interpreted two different ways.***

**Count and Ratio Matrices**

- The diagonal of the count and ratio matrices contain the number of
  samples containing the area of interest.

- Within each column, the returned counts or ratio values indicate the
  number (ratio) of samples within each area that contains both areas
  (the column and the row). The returned column values are always to be
  compared to the column of interest. The returned count will
  ***never*** be more than the diagonal value of the column.

- Each row, especially for the ratio matrices, should be interpreted as
  the similarity of each column to each other. For example, the ratios
  within a row

- Within each row, the returned counts or ratio values indicate the
  number (ratio) of samples within each area that contains both areas
  (the column and the row).

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
if (FALSE) { # \dontrun{
compare.dataset(data)
} # }
```
