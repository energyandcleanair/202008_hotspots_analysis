require(dplyr)
require(sf)
require(stringr)
require(rgdal) #install.packages("rgdal")
require(velox) #remotes::install_github("hunzikp/velox")
require(rgis) #remotes::install_github("Pakillo/rgis")
require(raster)
require(ggplot2)

source('./utils.R')


radii_km <- c(1,10,50)
pts_0 <- utils.read_points()

pts <- do.call("rbind", lapply(radii_km, utils.buffer, points=pts_0))


# Get values
files <- list.files("data",pattern="*.nc4", full.names = T)

pts_w_value <- do.call("rbind", lapply(files[1:3], utils.values_at_point, points=pts))

ggplot(tibble(pts_w_value) %>% dplyr::filter(NUMBER < 100)) + geom_line(aes(date, value, color='radius_km')) + facet_wrap(~NUMBER)
