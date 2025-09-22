

#' convertyXYZtrajectory2GEOJSON
#'
#' Function to convert xyz trajectory file to geojson
#' @param string input
#' @param string output
#' @keywords trajectory
#' @export
#' @examples
#' get_vuk_palette()
convertyXYZtrajectory2GEOJSON <- function(input, output){
  trj.raw <- read.csv(input, header = T, sep = " ")
  rand.x <- sample(c(1:nrow(trj.raw)), nrow(trj.raw)/100)
  trj.vec <- terra::vect(trj.raw[rand.x,],geom = c("x","y"), keepgeom = T)
  terra::writeVector(trj.vec,output,overwrite=TRUE)
}



 