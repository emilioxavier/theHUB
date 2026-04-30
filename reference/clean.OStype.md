# Get Operating System Name

Return the *generic* name of the operating system.

## Usage

``` r
clean.OStype(OperatingSystem)
```

## Arguments

- OperatingSystem:

  A string containing the operating system obtained by a Qualtrics
  survey; *e.g.*, "Windows NT 4.0 SP6a"

## Value

string with the *generic* operating system name; *e.g.*, "Windows"

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
clean.OStype("Windows NT 10.0")
#> [1] "Windows"
# "Windows"

clean.OStype("Windows NT 4.0 SP6a")
#> [1] "Windows"
# "Windows"

clean.OStype("CrOS x86_64 13505.63.0")
#> [1] "CrOS"
# "CrOS"
```
