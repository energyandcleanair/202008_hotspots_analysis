extract_omi_data <- function(){

  # Specific requirements to parallelize value extraction
  require(rgdal) #install.packages("rgdal")
  require(velox) #remotes::install_github("hunzikp/velox")
  require(rgis) #remotes::install_github("Pakillo/rgis")

  print("Creating OMI value files. Can take a few hours if not days...")
  radii_km <- c(2, 20, 200)
  d.measures.wide <- utils.read_points()

  pts <- do.call("rbind", lapply(radii_km, utils.buffer, points=d.measures.wide))

  # Get values
  files <- list.files("data",pattern="OMI-Aura_L3-OMSO2e_2020m08.*\\.nc4$", full.names = T)

  d <- do.call("rbind", pbapply::pblapply(files, utils.values_at_point, points=pts))
  d.omi <- d %>% dplyr::select(NUMBER, radius_km, value, date)

  saveRDS(d.omi, "value_202008.RDS")
}
