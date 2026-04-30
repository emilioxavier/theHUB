# Word Count

Count the number of words in a (text) string. A word is considered any
number of characters seperated by at least one physical space.

## Usage

``` r
word.count(comment)
```

## Arguments

- comment:

  string of interest

## Value

integer indicating the number of words in the provided string.

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
 word.count("today is awesome!")
#> [1] 3
 # 3

 word.count("no")
#> [1] 1
 # 1

 word.count("")
#> [1] 0
 # 0

 word.count("yes, 3 is a word.")
#> [1] 5
 # 5
```
