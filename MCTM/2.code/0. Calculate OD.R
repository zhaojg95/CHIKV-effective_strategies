
pkgs <- c("tmaptools","geosphere")
pacman::p_load(pkgs,character.only = T)

library(tmaptools)
library(geosphere)

od_dist_matrix <- function(coord,
                           mode = c("geo", "cheap"),
                           unit = c("m", "km", "mile")){
  
  mode  <- match.arg(mode)
  unit  <- match.arg(unit)
  coord <- as.matrix(coord)
  if(ncol(coord) != 2) stop("lon, lat")

  distFun <- switch(mode,
                    geo   = geosphere::distGeo,    
                    cheap = geosphere::distHaversine) 

  n <- nrow(coord)
  d <- matrix(0, n, n)
  d[lower.tri(d)] <- distFun(coord[lower.tri(d, TRUE)[,1], ],
                             coord[lower.tri(d, TRUE)[,2], ])
  d <- d + t(d)

  mult <- switch(unit, m = 1, km = 1e-3, mile = 0.000621371)
  d * mult
}


df <- read.xlsx("1.data/address.xlsx") 
row.names(df) <- df[,1]
df <- df[,c("lon","lat")]

(Dkm <- od_dist_matrix(df, unit = "km"))

write.xlsx(Dkm,"1.data/OD(fs).xlsx")



