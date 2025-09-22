library(terra)

trj.raw <- read.csv("Y:/Les/Lidar/MLS/DATA/BLUECAT_SITES/Zofin/2025/ZF_PAR_04-06-2025/ZF_PAR_02_traj.xyz", header = T, sep = " ")

rand.x <- sample(c(1:nrow(trj.raw)), nrow(trj.raw)/100)

trj.vec <- vect(trj.raw[rand.x,],geom = c("x","y"), keepgeom = T)

plot(trj.vec)

writeVector(trj.vec,"Y:/Les/Lidar/MLS/DATA/BLUECAT_SITES/Zofin/2025/ZF_PAR_04-06-2025/ZF_PAR_02_traj.geojson",overwrite=TRUE)


 