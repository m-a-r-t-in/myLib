scaleTo8bit <- function(rst)
{
  
  r.range <- range(na.omit(values(rst)))
  r.range.diff <- (r.range[2]-r.range[1])
  
  rst.8bit <- round(255*(rst-r.range[1])/r.range.diff , 0)
  
  return(rst.8bit)
  
}



scaleTo8bitRGB_adjusted <- function(rst,max_pt_density)
{
  
  rst <- reclassify(rst, c(max_pt_density,Inf,max_pt_density))
  rst <- reclassify(rst, c(NA,NA,0))
  
  r.range <- range(na.omit(values(rst)))
  r.range.diff <- (r.range[2]-r.range[1])
  
  rst <- round(255*(rst-r.range[1])/r.range.diff , 0)
  rst <- rst * (-1) + 255
  
  rst.8bit.RGB <- stack(rst,rst,rst)
  names(rst.8bit.RGB) <- c("R","G","B")
  
  return(rst.8bit.RGB)
  
}

# writeRaster(rst,"S:/GeoData/PRACOVNI_LES/TLS_ground_VUKOZ/Zofin_2022/8bit/TLS_23.tif",datatype='INT1U', overwrite=TRUE)