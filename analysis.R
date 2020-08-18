require(dplyr)
require(sf)
require(stringr)
require(pbapply)
require(raster)
require(rgdal) #install.packages("rgdal")
require(velox) #remotes::install_github("hunzikp/velox")
require(rgis) #remotes::install_github("Pakillo/rgis")



source('./utils.R')


radii_km <- c(2, 20, 200)
pts_0 <- utils.read_points()

pts <- do.call("rbind", lapply(radii_km, utils.buffer, points=pts_0))

# Get values
files <- list.files("data",pattern="\\.nc4$", full.names = T)

pt_values <- do.call("rbind", pbapply::pblapply(files, utils.values_at_point, points=pts))
pt_values_lite <- pt_values %>% dplyr::select(NUMBER, radius_km, value, date)

saveRDS(pt_values_lite, file.path("results", "values.RDS"))


