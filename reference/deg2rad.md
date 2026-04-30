# Calculate Latitude-Longitude Distance Constants

Constants needed to calculate the distance between two latitude-
longitude points on a globe, aka earth.

- `deg2rad` \| Degrees to Radians: `0.0174532925199433` for the
  conversion of degrees (specifically latitude and longitude values) to
  radians. Needed for calculating the distance between two locations on
  the globe using latitude-longitude values.

\\ deg2rad = \frac{\pi}{180} \\

- `earth.radius.miles` \| Earth's Radius: `3963.17` miles.

- `earth.radius.km` \| Earth's Radius: `6373` kilometers.

## Usage

``` r
deg2rad

earth.radius.miles

earth.radius.km
```

## Format

numeric (double)

An object of class `numeric` of length 1.

An object of class `numeric` of length 1.

## Functions

- `earth.radius.miles`: Earth's Radius

- `earth.radius.km`: Earth's Radius

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)
