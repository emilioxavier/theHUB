# Longitude-Latitude to Region (version 2)

Identify the region (and subregion) of a geospatial location position
using longitude and latitude.

## Usage

``` r
lonlat2region.2(data, longitude, latitude, shapedata)
```

## Arguments

- data:

  location data/information

- longitude:

  numeric value indicating the longitude

- latitude:

  numeric value indicating the latitude

- shapedata:

  Mapfile can be used, but for datasets containing geospatial location
  data from anywhere on the planet, it is advantageous to use the entire
  world map. Obtain the mapfile of interest from <https://gadm.org>. The
  world GADM data is available from
  https://biogeo.ucdavis.edu/data/gadm3.6/gadm36_gpkg.zip
  (https://biogeo.ucdavis.edu/data/gadm3.6/gadm36_gpkg.zip). Ensure the
  [GeoPackage format](https://gadm.org/formats.html) is downloaded. The
  compressed complete world database is approximately 1.6 GBs and the
  unzipped file is 3.81 GBs. For example,
  `~/mapfiles/gadm36_levels.gpkg`

  Individual country maps are available from
  <https://gadm.org/maps.html>.

## Value

tibble with the country.code, country.name, region, and subregion.

## Details

Based on the provided longitude and latitude, the corresponding country,
region, and subregion are identified. Positions outside of a country
return `NA`s for the country.code, country.name, region, and subregion.
Instances where `NA` is provided for longitude and latitude, also
returns `NA` values.

When using the world GADM database, loading the rather large world
database takes time and, thus, the function appears to be slow.

This function is based on the ["Latitude Longitude Coordinates to State
Code in
R"](https://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r)
stackoverflow response by [Josh
O'Brien](https://stackoverflow.com/users/980833/josh-obrien).

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)

## Examples

``` r
if (FALSE) { # \dontrun{
mapfile.world <- "~/mapfiles/gadm36_levels.gpkg"
lonlat2region.2(data, longitude, latitude, gadm.mapfile=mapfile.world)
} # }
```
