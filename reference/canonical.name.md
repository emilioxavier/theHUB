# Make Canonical Name

Often there is a need to construct a name from several different
components. While a common method to construct these names from the
provided components in the provided order, it can lead to different
names for the same thing.

If the name for an object is composed from two or more columns there is
the possibility that the components change order within the columns. In
some cases the name "yaddy-blah" is the same as "blah-yaddy." This
function takes the provided components, replaces `NA`s with "Unk",
orders the components alphabetically and in numerical order, and then
combines the components with "-"s.

## Usage

``` r
canonical.name(name.vector)
```

## Arguments

- name.vector:

  A string containing the components to comprise the canonical name.

## Value

string with the canonical name

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
canonical.name(c("yaddy", "yaddy", "blah", "ugh"))
#> [1] "blah-ugh-yaddy-yaddy"

if (FALSE) { # \dontrun{
  ## the following is how to implement the `canonical.name()` via `dplyr::mutate()`
  mutate(name=canonical.name(c(column.1, column.2)))

} # }
```
