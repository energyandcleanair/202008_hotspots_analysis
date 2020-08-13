require(dplyr)
require(sf)
require(stringr)
require(velox) #remotes::install_github("hunzikp/velox")
require(rgis) #remotes::install_github("Pakillo/rgis")


source('./utils.R')


radii_km <- c(1,10,50)
pts_0 <- utils.read_points()

pts <- do.call("rbind", lapply(radii_km, utils.buffer, points=pts_0))


# Get values
files <- list.files("data",pattern="*.nc4", full.names = T)


utils.values_at_point(pts, files[1])

