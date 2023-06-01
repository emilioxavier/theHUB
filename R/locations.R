

#' @title Latitude-Longitude Distances
#' @description Calculate distance between points on the globe
#' @details long description with what the function does and returns
#'
#' @param lat1 latitude value for the first set of latitude-longitude coordinates
#' @param lon1 longitude value for the first set of latitude-longitude coordinates
#'   _e.g._, c(lat1, lon1), c(42.331177, -83.046023)  ## Detroit, Michigan
#' @param lat2 latitude value for the second set of latitude-longitude coordinates
#' @param lon2 longitude value for the second set of latitude-longitude coordinates
#'   _e.g._, c(lat2, lon2), c(42.737652, -84.483788)  ## East Lansing, Michigan
#' @param distance.units string indicating if the resulting distance should be
#'   miles or kilometers expressed as `"km"`; default is `"miles"`.
#'
#'
#' @return distance between the two points taking into consideration the curvature of the earth
#'
#' @export
#'
#' @examples
#' point1 <- c(42.331177, -83.046023)  ## Detroit, Michigan
#' point2 <- c(42.737652, -84.483788)  ## East Lansing, Michigan
#'
#' LatLonDistances(lat1=point1[1], lon1=point1[2],
#'                 lat2=point2[1], lon2=point2[2], distance.units="miles")
#' # 78.48957
#'
#' LatLonDistances(lat1=42.331177, lon1=-83.046023,
#'                 lat2=42.737652, lon2=-84.483788, distance.units="miles")
#' # 78.48957
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
# LatLonDistances <- function(point1, point2, distance.units="miles") {
LatLonDistances <- function(lat1, lon1, lat2, lon2, distance.units="miles") {

  ## earth's radius | miles or km? ----
  earth.radius <- theHUB::earth.radius.miles
  if ( tolower(distance.units) == "km" ) {
    earth.radius <- theHUB::earth.radius.km
  }

  ## convert degrees to radian ----
  # deg2rad <- pi/180
  lat1 <- lat1 * theHUB::deg2rad
  lat2 <- lat2 * theHUB::deg2rad
  lon1 <- lon1 * theHUB::deg2rad
  lon2 <- lon2 * theHUB::deg2rad

  ## extract latitudes and longitudes ----
  # lat1 <- point1[1]
  # lon1 <- point1[2]
  #
  # lat2 <- point2[1]
  # lon2 <- point2[2]

  ## calculated differences between latitude and longitude points ----
  dist.lat <- (lat2 - lat1)
  dist.lon <- (lon2 - lon1)

  ## calculate the difference between the two points ----
  a <- ( sin(dist.lat/2) )^2 + cos(lat1) * cos(lat2) * ( sin(dist.lon/2) )^2
  # c <- 2 * atan2(sqrt(a), sqrt(1-a))
  #  d <- R * c ##(where earth.radius is the radius of the Earth; 3961 or 3963.17 miles & 6373 km)
  distance <- 2 * earth.radius * asin(sqrt(a))

  ## return the distance ----
  return(distance)
}


#' @title Longitude-Latitude to Region
#'
#' @description Identify the region (and subregion) of a geospatial location
#'   position using longitude and latitude.
#'
#' @details Based on the provided longitude and latitude, the corresponding
#'   country, region, and subregion are identified. Positions outside of a
#'   country return `NA`s for the country.code, country.name, region, and
#'   subregion. Instances where `NA` is provided for longitude and latitude,
#'   also returns `NA` values.
#'
#'   When using the world GADM database, loading the rather large world database
#'   takes time and, thus, the function appears to be slow.
#'
#'   This function is based on the ["Latitude Longitude Coordinates to State Code in R"](https://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r)
#'   stackoverflow response by
#'   [Josh O'Brien](https://stackoverflow.com/users/980833/josh-obrien).
#'
#' @param longitude numeric value indicating the longitude
#' @param latitude numeric value indicating the latitude
#' @param gadm.mapfile string with the path and file name of the GADM world
#'   mapfile. Any mapfile can be used, but for datasets containing geospatial
#'   location data from anywhere on the planet, it is advantageous to use the
#'   entire world map. Obtain the mapfile of interest from
#'   [https://gadm.org](https://gadm.org). The world GADM data is available from
#'   [https://biogeo.ucdavis.edu/data/gadm3.6/gadm36_gpkg.zip]
#'   (https://biogeo.ucdavis.edu/data/gadm3.6/gadm36_gpkg.zip).
#'   Ensure the [GeoPackage format](https://gadm.org/formats.html) is downloaded.
#'   The compressed complete world database is approximately 1.6 GBs and the
#'   unzipped file is 3.81 GBs. For example, `~/mapfiles/gadm36_levels.gpkg`
#'
#'    Individual country maps are available from
#'    [https://gadm.org/maps.html](https://gadm.org/maps.html).
#'
#' @importFrom sf st_read st_transform st_as_sf st_intersects
#' @importFrom tibble as_tibble tibble
#'
#' @return tibble with the country.code, country.name, region, and subregion.
#' @export
#'
#' @examples
#' \dontrun{
#' mapfile.world <- "~/mapfiles/gadm36_levels.gpkg"
#' lonlat2region(longitude, latitude, gadm.mapfile=mapfile.world)
#' }
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
lonlat2region <- function(longitude, latitude, gadm.mapfile) {

  ## make a tibble with longitude and latitude ----
  points.oi <- tibble::tibble(longitude=longitude,
                              latitude=latitude)

  ## load world GAMD file ----
  gamd.world <- sf::st_read(dsn=gadm.mapfile, layer="level2", quiet=TRUE)
  ##_ extract country code, country name, region, and subregion data ----
  region.names <- tibble::as_tibble(gamd.world[, c("GID_0", "NAME_0", "NAME_1", "NAME_2")])
  region.names <- select(region.names, -"geom")

  ## convert points.oi tibble to an sf POINTS object ----
  points.oi.4326 <- sf::st_as_sf(points.oi, coords=1:2, crs=4326, na.fail=FALSE)

  ## transform spatial data to some planar coordinate system ----
  ## (e.g. Web Mercator) as required for geometric operations
  region.crs3857 <- sf::st_transform(gamd.world, crs=3857)
  points.oi.crs3857 <- sf::st_transform(points.oi.4326, crs=3857)

  ## identify regions intersecting with each point, if overlap possible ----
  region.idc <- as.integer(sf::st_intersects(points.oi.crs3857, region.crs3857))
  region.names.oi <- region.names[region.idc, ]

  ## clean-up column names ----
  colnames(region.names.oi) <- c("country.code", "country.name", "region", "subregion")

  ## return region names of interest ----
  return(region.names.oi)
}


#' @title Longitude-Latitude to Region (version 2)
#'
#' @description Identify the region (and subregion) of a geospatial location
#'   position using longitude and latitude.
#'
#' @details Based on the provided longitude and latitude, the corresponding
#'   country, region, and subregion are identified. Positions outside of a
#'   country return `NA`s for the country.code, country.name, region, and
#'   subregion. Instances where `NA` is provided for longitude and latitude,
#'   also returns `NA` values.
#'
#'   When using the world GADM database, loading the rather large world database
#'   takes time and, thus, the function appears to be slow.
#'
#'   This function is based on the ["Latitude Longitude Coordinates to State Code in R"](https://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r)
#'   stackoverflow response by
#'   [Josh O'Brien](https://stackoverflow.com/users/980833/josh-obrien).
#'
#' @param data location data/information
#' @param longitude numeric value indicating the longitude
#' @param latitude numeric value indicating the latitude
#' @param shapedata Mapfile can be used, but for datasets containing geospatial
#'   location data from anywhere on the planet, it is advantageous to use the
#'   entire world map. Obtain the mapfile of interest from
#'   [https://gadm.org](https://gadm.org). The world GADM data is available from
#'   [https://biogeo.ucdavis.edu/data/gadm3.6/gadm36_gpkg.zip]
#'   (https://biogeo.ucdavis.edu/data/gadm3.6/gadm36_gpkg.zip).
#'   Ensure the [GeoPackage format](https://gadm.org/formats.html) is downloaded.
#'   The compressed complete world database is approximately 1.6 GBs and the
#'   unzipped file is 3.81 GBs. For example, `~/mapfiles/gadm36_levels.gpkg`
#'
#'   Individual country maps are available from
#'   [https://gadm.org/maps.html](https://gadm.org/maps.html).
#'
#' @importFrom sf st_read st_transform st_as_sf st_intersects
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyselect starts_with
#'
#' @return tibble with the country.code, country.name, region, and subregion.
#' @export
#'
#' @examples
#' \dontrun{
#' mapfile.world <- "~/mapfiles/gadm36_levels.gpkg"
#' lonlat2region.2(data, longitude, latitude, gadm.mapfile=mapfile.world)
#' }
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
# data <- location.data; longitude <- "LocationLongitude"; latitude <- "LocationLatitude"; shapedata <- world.shape.data
lonlat2region.2 <- function(data, longitude, latitude, shapedata) {

  ## make a tibble with longitude and latitude ----
  # points.oi <- tibble::tibble(longitude=data[[longitude]],
  #                             latitude=data[[latitude]])

  ## convert points.oi tibble to an sf POINTS object ----
  # points.oi.4326 <- sf::st_as_sf(points.oi, coords=1:2, crs=4326, na.fail=FALSE)
  points.oi.crs4326 <- sf::st_as_sf(data[, c(longitude, latitude)], coords=1:2, crs=4326, na.fail=FALSE)

  ## transform spatial data to some planar coordinate system ----
  ## (e.g. Web Mercator) as required for geometric operations
  region.crs3857 <- sf::st_transform(shapedata, crs=3857)
  points.oi.crs3857 <- sf::st_transform(points.oi.crs4326, crs=3857)

  ## identify regions intersecting with each point, if overlap possible ----
  region.idc <- as.integer(sf::st_intersects(points.oi.crs3857, region.crs3857))
  region.names.oi <- shapedata[region.idc, ]

  ## clean-up column names ----
  region.names.tb <- tibble::as_tibble(region.names.oi)
  region.names.tb <- dplyr::select(region.names.tb, -starts_with("geom"))

  ## return region names of interest ----
  return(region.names.tb)
}

