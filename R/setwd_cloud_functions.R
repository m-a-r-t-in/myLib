#' SETWD_MEGA
#'
#' Set working directory to MEGAsync folder, operating system independent
#' @param folder Subfolder in MEGAsync
#' @keywords setwd
#' @export
#' @examples
#' setwd_mega()

setwd_mega <- function(folder = "none"){
  s.i <- Sys.info()

  if(s.i[1]=="Windows"){
    path.mega = "C:/Users/krucek/Documents/MEGAsync"
    print("Operating system = WINDOWS")
  }else if(s.i[1]=="Darwin"){
    path.mega = "/Users/martinkrucek/MEGAsync"
    print("Operating system = MAC")
  }else{
    print("ERROR !     Cannot determine operating system.")
    print("Working directory is not set.")
    return()
  }

  if(folder != "none"){
    path.mega <- paste(path.mega,"/",folder, sep = "")
  }

  setwd(path.mega)
  print(paste("Working directory =",path.mega))
  return()
}


#' SETWD_ICLOUD
#'
#' Set working directory to iCloud folder, operating system independent
#' @param folder Subfolder in iCloud
#' @keywords setwd
#' @export
#' @examples
#' setwd_icloud()

setwd_icloud <- function(folder = "none"){
  s.i <- Sys.info()

  if(s.i[1]=="Windows"){
    path.icloud = "C:/Users/krucek/iCloudDrive"
    print("Operating system = WINDOWS")
  }else if(s.i[1]=="Darwin"){
    path.icloud = "/Users/martinkrucek/Library/Mobile Documents/com~apple~CloudDocs"
    print("Operating system = MAC")
  }else{
    print("ERROR !     Cannot determine operating system.")
    print("Working directory is not set.")
    return()
  }

  if(folder != "none"){
    path.mega <- paste(path.icloud,"/",folder, sep = "")
  }
  setwd(path.icloud)
  print(paste("Working directory =",path.icloud))
  return()
}


#' SET_ONEDRIVE
#'
#' Set working directory to OneDrive folder, operating system independent
#' @param folder Subfolder in OneDrive
#' @keywords setwd
#' @export
#' @examples
#' set_onedrive()

set_onedrive <- function(folder = "none"){
  s.i <- Sys.info()

  if(s.i[1]=="Windows"){
    path= "C:/Users/krucek/OneDrive - vukoz.cz"
    print("Operating system = WINDOWS")
  }else if(s.i[1]=="Darwin"){
    path = "/Users/martinkrucek/Library/CloudStorage/OneDrive-vukoz.cz"
    print("Operating system = MAC")
  }else{
    print("ERROR !     Cannot determine operating system.")
    print("Working directory is not set.")
    return()
  }

  if(folder != "none"){
    path <- paste(path,folder, sep = "/")
  }
  setwd(path)
  print(paste("Working directory =",path))
  return()
}



