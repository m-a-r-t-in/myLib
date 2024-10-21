
#' raster_asc2tif
#'
#' convert all ascii rasters in folder into tif
#' @param path directory
#' @keywords convert rasters
#' @export
#' @examples
#' raster_asc2tif()

raster_asc2tif <- function(path){

  print(path)

  if(dir.exists(path)){

    print("IF")

    files <- dir(path)
    rst.write.status <- c()
    print(files)

    for(f in files){

      f.type <- substring(f,nchar(f)-3,nchar(f))
      print(f.type)

      if(f.type == ".asc"){

        f.path.asc <- paste(path,f, sep = "/")
        f.path.tif <- gsub(".asc",".tif",f.path.asc)
        print(f.path.asc)
        print(f.path.tif)
        raster::writeRaster(raster::raster(f.path.asc), f.path.tif, overwrite = T )
        rst.write.status <- append(rst.write.status,TRUE)

      }else rst.write.status <- append(rst.write.status,FALSE)
    }
    return(rst.write.status)
  }else{
      print("ELSE")
      f.path.tif <- gsub(".asc",".tif",path)
      raster::writeRaster(raster::raster(path), f.path.tif, overwrite = T )
      return(TRUE)
  }
  return(FALSE)
}



