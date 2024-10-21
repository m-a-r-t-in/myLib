
#' sdf
#'
#' Function to view SpatVector attribute table
#' @param SpatVector object or any other object that can be converted by as.data.frame fci
#' @keywords show as data.frame
#' @export
#' @examples
#' sdf()
sdf <- function(x) {

  if(inherits(x,"SpatVector")){
    View(as.data.frame(x, geom = "WKT"))
  }else{
    View(as.data.frame(x))
  }

}

