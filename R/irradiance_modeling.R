#' get.julian.day.time
#'
#' convert date from "YYYY-MM-DDTHH-MM-SS" to julian
#' @param date
#' @keywords julian date convert
#' @export
#' @examples
#' get.julian.day.time()
get.julian.day.time <- function(date){
  y <- as.double(substring(date,1,4))
  mo <- as.double(substring(date,6,7))
  d <- as.double(substring(date,9,10))
  h <- as.double(substring(date,12,13))
  mi <- as.double(substring(date,15,16))
  s <- as.double(substring(date,18,19))
  return(JDymd(y,mo,d,h,mi,s))
}

#' load.meteodata.csv
#'
#' load meteodata as dataframe from folder containing CSV files
#' work for ZF FGEO meteostation data where single file represents one month
#' merge all csv files from folder in one dataframe
#' @param folder
#' @keywords meteodata
#' @export
#' @examples
#' load.meteodata.csv()
load.meteodata.csv <- function(folder){
  files <- dir(folder)
  meteo <- read.csv(paste(folder,files[1], sep = "/"), stringsAsFactors = FALSE)
  
  for(i in 2:length(files)){
    m <- read.csv(paste(folder,files[i], sep = "/"))
    meteo <- rbind(meteo,m)
  }
  
  return(meteo)
}

#' model.potential.irradiance.J 
#'
#' model potential incoming irradiance in joules at given date
#' @param plot.constants <- c(latitude,longitude,altitude,timezone)
#' @param date.variables <- c(julian.date,visibility.km,humidity,temperature.K,ozone.thickness,albedo)
#' @keywords irradiance
#' @export 
#' @examples
#' model.potential.irradiance.J ()
model.potential.irradiance.J <- function(plot.constants, date.variables ){
  
  sun.direction = sunvector(date.variables[1],plot.constants[1],plot.constants[2],plot.constants[4])
  zenith = sunpos(sun.direction)[2]
  # compute direct[,1] & diffuse[,2] irad
  I.direct.diffuse = insolation(zenith,date.variables[1],plot.constants[3],date.variables[2],date.variables[3],date.variables[4],date.variables[5],date.variables[6])
  return(I.direct.diffuse)
}


#' compute.irradiance.on.surface 
#'
#' compute irradiance on real surface
#' @param potential.irradiance.J
#' @param surface.normals
#' @param surface.matrix
#' @param plot.constants c(latitude,longitude,altitude,timezone)
#' @param date.variables c(julian.date,visibility.km,humidity,temperature.K,ozone.thickness,albedo)
#' @param time.interval.sec
#' @keywords irradiance
#' @export 
#' @examples
#' model.irradiance ()
compute.irradiance.on.surface <- function(potential.irradiance.J, surface.normals, surface.matrix, plot.constants, date.variables, time.interval.sec,raster.resolution ){
  
  sun.direction = sunvector(date.variables[1],plot.constants[1],plot.constants[2],plot.constants[4])
  #compute intenisty of illumination
  hsh = hillshading(surface.normals,sun.direction)
  #compute visibility matrix for direct irad
  sh = doshade(surface.matrix,sun.direction,raster.resolution)
  # Create empty matrices in size of raster matrix
  I.global = array(0,dim=dim(surface.matrix))
  I.direct = array(0,dim=dim(surface.matrix))
  # sum irad for given time period
  I.global = I.global + (potential.irradiance.J[,1] * hsh * sh + potential.irradiance.J[,2] )*time.interval.sec
  I.direct = I.direct +  (potential.irradiance.J[,1] * hsh * sh)*time.interval.sec
  
  return(list(I.global,I.direct))
}




