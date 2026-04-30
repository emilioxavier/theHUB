# Latitude-Longitude Distances

Calculate distance between points on the globe

## Usage

``` r
LatLonDistances(lat1, lon1, lat2, lon2, distance.units = "miles")
```

## Arguments

- lat1:

  latitude value for the first set of latitude-longitude coordinates

- lon1:

  longitude value for the first set of latitude-longitude coordinates
  *e.g.*, c(lat1, lon1), c(42.331177, -83.046023) \## Detroit, Michigan

- lat2:

  latitude value for the second set of latitude-longitude coordinates

- lon2:

  longitude value for the second set of latitude-longitude coordinates
  *e.g.*, c(lat2, lon2), c(42.737652, -84.483788) \## East Lansing,
  Michigan

- distance.units:

  string indicating if the resulting distance should be miles or
  kilometers expressed as `"km"`; default is `"miles"`.

## Value

distance between the two points taking into consideration the curvature
of the earth

## Details

long description with what the function does and returns

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
point1 <- c(42.331177, -83.046023)  ## Detroit, Michigan
point2 <- c(42.737652, -84.483788)  ## East Lansing, Michigan

LatLonDistances(lat1=point1[1], lon1=point1[2],
                lat2=point2[1], lon2=point2[2], distance.units="miles")
#> [1] 78.48957
# 78.48957

LatLonDistances(lat1=42.331177, lon1=-83.046023,
                lat2=42.737652, lon2=-84.483788, distance.units="miles")
#> [1] 78.48957
# 78.48957
```
