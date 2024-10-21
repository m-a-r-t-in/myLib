#' raster_UTM33NtoSJTSK
#'
#' Project raster UTM33N -> SJTSK, EPSG: 32633 -> 5514
#' @param raster raster to be projected
#' @keywords raster
#' @export
#' @examples
#' raster_UTM33NtoSJTSK()
raster_UTM33NtoSJTSK <- function(input.raster){

  raster::crs(input.raster) <- sp::CRS('+init=EPSG:32633')
  projected <- raster::projectRaster(input.raster, crs = '+init=EPSG:5514')
  return(projected)

}
#' raster_SJTSKtoUTM33N
#'
#' Project raster SJTSK -> UTM33N, EPSG:  5514 -> 32633
#' @param raster raster to be projected
#' @keywords raster
#' @export
#' @examples
#' raster_SJTSKtoUTM33N()
raster_SJTSKtoUTM33N <- function(input.raster){

  raster::crs(input.raster) <- sp::CRS('+init=EPSG:5514')
  projected <- raster::projectRaster(input.raster, crs = '+init=EPSG:32633')
  return(projected)

}

#' move_point_xy
#'
#' move point xy by vector given by azimuth and distance
#' @param x coord of point
#' @param y coord of point
#' @param azimuth in degrees
#' @param distance in map units
#' @keywords point
#' @export
#' @examples
#' move_point_xy()
move_point_xy <- function(x,y,azimuth,distance){

  radians <- azimuth * pi / 180

  x_adj <- x + distance * sin(radians)
  y_adj <- y + distance * cos(radians)

  return(c(x_adj,y_adj))

}


