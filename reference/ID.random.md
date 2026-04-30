# Create Collection of Random IDs

Given a collection of IDs, create random IDs from a large pool of
potential IDs. The function can create random IDs comprised of number,
letters, or both letters-and-numbers referred to as mixed for short.

Random IDs comprised of numbers include leading zeros (*e.g.*, `0013`).

Random IDs comprised of letters and letters-and-numbers (aka mixed) are
20 characters long and contain a mixture of upper and lower case
letters.

## Usage

``` r
ID.random(ID, ID.type = "mixed")
```

## Arguments

- ID:

  string with ID or collection of IDs

- ID.type:

  string indicating the type of random IDs to generate; default:
  `mixed`. Three options are available. 1. `numbers`, 2. `letters`,
  and 3. `mixed` (aka, letters-and-numbers). In all instances, random
  IDs with letters contain a mixture of upper and lower case letters.

## Value

string vector

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

Seth Walker <walker893@msu.edu> (<https://github.com/walker893>)

## Examples

``` r
IDs <- c("1234500000", "12345", NA, 12345, 1234.56, "puzzle", "00011111")

set.seed(13)
ID.random(ID=IDs, ID.type="numbers")
#> [1] "4870" "5706" "0717" "0944" "5989" "5592" "0960"
# [1] "4870" "5706" "0717" "0944" "5989" "5592" "0960"

ID.random(ID=IDs, ID.type="letters")
#> [1] "XWEvKQIsaCtOebfTOQLT" "uGUyOZBtcXwgRNbiBChq" "TTmJCbNheHsUQsDnTQni"
#> [4] "oRunjWsEvOlsaHFwqVUk" "eBnpYgroTrWIygAeYoBw" "HlUVBpkyMDGOowcXaMhX"
#> [7] "UIdWmaXZXTqjMzJRczxs"
# [1] "XWEvKQIsaCtOebfTOQLT" "uGUyOZBtcXwgRNbiBChq" "TTmJCbNheHsUQsDnTQni"
# "oRunjWsEvOlsaHFwqVUk" "eBnpYgroTrWIygAeYoBw" "HlUVBpkyMDGOowcXaMhX"
# "UIdWmaXZXTqjMzJRczxs"

ID.random(ID=IDs, ID.type="mixed")
#> [1] "HL0WXSdG8sf5jFIJXbuz" "fUGVPSvRYS7l5LvsEjmH" "t3GCKf3JkZDgb2kuq6Be"
#> [4] "NXC9vLfKTV7tqwS3ss7F" "AywRolRrH4Myl5zvXCjA" "H0DLeZ7AGQp8RGDhLSoV"
#> [7] "vVTaHCI0rxVlJ3AU9wrK"
# [1] "HL0WXSdG8sf5jFIJXbuz" "fUGVPSvRYS7l5LvsEjmH" "t3GCKf3JkZDgb2kuq6Be"
# "NXC9vLfKTV7tqwS3ss7F" "AywRolRrH4Myl5zvXCjA" "H0DLeZ7AGQp8RGDhLSoV"
# "vVTaHCI0rxVlJ3AU9wrK"
```
