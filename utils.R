utils.read_points <- function(){

  p <- readxl::read_xls(file.path("data", "OMI_Catalogue_Emissions_2005-2019.xls"),
                        range="A3:BQ591")

  s <- sf::st_as_sf(p, coords=c("LONGITUD", "LATITUDE"), crs="+proj=longlat +datum=WGS84") #Same crs as raster to avoid transformations

  return(s)
}

utils.buffer <- function(radius_km, points){

  if(is.null(radius_km) | radius_km==0) return(points %>% dplyr::mutate(radius_km=radius_km))

  crs_org = sf::st_crs(points)
  # Convert to EPSG:3857 to have a meter unit
  sf::st_transform(points, crs=3857) %>%
    sf::st_buffer(radius_km*1000) %>%
    sf::st_transform(crs=crs_org) %>%
    dplyr::mutate(radius_km=radius_km)
}

utils.date_from_filename <- function(filename){
  ymd <- sub(".*OMSO2e_([0-9]{4})m([0-9]{4}).*","\\1\\2", filename, perl=T)
  return (as.POSIXct(ymd, format="%Y%m%d", tz="UTC"))
}

utils.values_at_point <- function(file, points){
  r <- brick(file, varname="ColumnAmountSO2_PBL")
  values <- raster::extract(r, points)[,1]

  points$value <- values
  points$date <- utils.date_from_filename(file)
  return(points)
}
