#create GeoJSON contour polygons out of a raster map
source('map/R_init.R')

loadlibs(online=F)

setwd(boxpath("AOD/2012-2017dailySO2,NO2"))
list.files(pattern='scrubbed.*\\.nc$') -> giovanni
giovanni %<>% colsplit('\\.', c('bird', 'var', 'date', 'ext')) %>% data.frame(name=giovanni, .) %>%
  mutate_at('date', ymd)

list.files(pattern='OMI-Aura_L3.*\\.nc4$') -> ssw
ssw %<>% colsplit('_', c('bird', 'var', 'date', 'ext')) %>% data.frame(name=ssw, .) %>%
  mutate_at('date', ymd)

list.files(pattern='OMI-Aura_L3.*\\.he5$') -> oa
oa %<>% colsplit('_', c('bird', 'var', 'date', 'ext')) %>% data.frame(name=oa, .) %>%
  mutate_at('date', ymd)

ssw$name[1] %>% as.character %>% raster %>% raster -> gridR
oa$name %>% as.character() %>%
  lapply(function(f) {
    f %>% raster(varname='Data Fields/ColumnAmountNO2TropCloudScreened') -> r
    extent(r) <- extent(gridR)
    crs(r) <- crs(gridR)
    writeRaster(r, gsub('\\.he5', '.grd', f), overwrite=T)
  })

oa$name %<>% gsub('\\.he5', '.grd', .)

bind_rows(giovanni, ssw, oa) -> inF
inF$var[grep('NO2', inF$var)] <- 'NO2'
inF$var[grep('SO2', inF$var)] <- 'SO2'
inF %<>% distinct(date, var, .keep_all = T)

inF %<>% dlply(.(var))

file2018 <- file.path("map","GIOVANNI-g4.timeAvgMap.OMSO2e_003_ColumnAmountSO2_PBL.20180101-20181231.180W_90S_180E_90N.tif")
file2019 <- file.path("map","GIOVANNI-g4.timeAvgMap.OMSO2e_003_ColumnAmountSO2_PBL.20190101-20191231.180W_90S_180E_90N.tif")


file=file2019
year=2019


process.file <- function(file, year){

  r <- raster::raster(file)


  levels <- c(0, 0.05, seq(0.1, 0.5, 0.1),
              0.75,1)  #specify contour levels


  contours <- raster::rasterToContour(r, maxpixels = Inf,
                                      levels=levels)

  contours.pol <- st_polygonize(st_as_sf(contours))

  #identify types of peaks
  contours.pols <- contours.pol %>% st_collection_extract("POLYGON")
  contours.pols$level %<>% as.character() %>% as.numeric()

  contours.pols %<>% filter(level>0)
  contours.pols %<>% rename(value=level)

  # Remove tiny polygons
  contours.pols %<>% subset(st_area(.) > units::set_units(1E9, "m^2"))




  write_sf(contours.pols, dsn = file.path("map",paste0("contour.",year,".geojson")), delete_dsn = TRUE)

  # write_sf(contours.pols %>% st_simplify(dTolerance=0.01, preserveTopology=T),
  #          dsn = file.path("map",paste0("contour.",year,".01.geojson")), delete_dsn = TRUE)
  #
  # write_sf(contours.pols %>% st_simplify(dTolerance=0.001, preserveTopology=T),
  #          dsn = file.path("map",paste0("contour.",year,".001.geojson")), delete_dsn = TRUE)

}





mean.noextremes <- function(x, probs=c(0, .9)) {
  x[x<0]<-0
  #quantile(x, probs=probs[1], na.rm=T) -> q.low
  #x[x<q.low] <- q.low
  quantile(x, probs=probs[2], na.rm=T) -> q.high
  x[x>q.high] <- q.high
  mean(x, na.rm=T)
}

#get power plant data
readRDS('power plant sites.RDS') -> pp.sites


#get NASA catalogue data
"air pollution/Eskom/hotspot report/OMI_Catalogue_Emissions_2005-2019_ILS-update.xlsx" %>%
  boxpath() %>%
  read_xlsx(skip=2, .name_repair = make.names) %>% spdf %>% st_as_sf -> ps


#process and output contours for year ranges
for(dates in dates.list) {
  inF$SO2 %>% filter(date %in% dates) %>%
    use_series(name) %>% stack -> omiall

  rF <- paste0('OMI SO2 ',min(dates),'to',max(dates),'.grd')
  if(file.exists(rF)) { r <- raster(rF)
  } else omiall %>% calc(fun=mean.noextremes, filename=rF) -> r


  brks <- c(0.06, seq(0.075, .150, 0.025),
            seq(0.2, 0.4, 0.1),
            seq(0.6, 1.2, 0.2)) %>%  #specify contour levels
    round(4)

  r %>% focalcircle(.499) -> r.plot

  #convert to contour lines
  rasterToContour(r.plot, maxpixels = Inf,
                  levels=brks) ->
    clines

  st_as_sf(clines) -> clines.sf
  st_polygonize(clines.sf) -> cpols



  #identify types of peaks
  cpols %<>% st_collection_extract("POLYGON")

  #volcanic
  st_intersects(ps %>% subset(SOURCETY == "Volcano"),
                cpols, sparse = F) -> ints
  cpols$NASA.volcanic <- ints %>% apply(2, any)
  st_intersects(ps %>% subset(grepl('Etna', NASA.NAME)),
                cpols, sparse = F) -> ints
  cpols$NASA.Etna <- ints %>% apply(2, any)

  st_contains(cpols %>% subset(NASA.volcanic),
              cpols, sparse = F) -> ints
  cpols$NASA.volcanic.offcenter <- ints %>% apply(2, any)

  #NASA anthropogenic
  st_intersects(ps %>% subset(SOURCETY != "Volcano"),
                cpols, sparse = F) -> ints
  cpols$NASA.manmade <- ints %>% apply(2, any)
  st_contains(cpols %>% subset(NASA.manmade),
              cpols, sparse = F) -> ints
  cpols$NASA.manmade.offcenter <- ints %>% apply(2, any)

  #coal plants
  st_intersects(pp.sites, cpols, sparse = F) -> ints
  cpols$Power.Plant <- ints %>% apply(2, any)
  pp.sites$SO2.detected <- ints %>% apply(1, any)
  st_contains(cpols %>% subset(Power.Plant),
              cpols, sparse = F) -> ints
  cpols$Power.Plant.offcenter <- ints %>% apply(2, any)

  cpols$include <- F
  cpols$include[cpols$NASA.manmade] <- T
  cpols$include[cpols$Power.Plant & !cpols$NASA.volcanic] <- T
  cpols$include[(cpols$NASA.manmade.offcenter |
                    cpols$Power.Plant.offcenter) &
                   !cpols$NASA.volcanic &
                   !cpols$NASA.volcanic.offcenter] <- T
  cpols$include[!cpols$NASA.manmade & cpols$NASA.Etna] <- F

  #exclude volcanic peaks
  cpols %>% subset(include) -> cpols2

  #remove very large polygons
  cpols2 %<>% subset(st_area(.) < units::set_units(3e12, "m^2"))

  #aggregate back to multipolygons
  cpols2 %>% split(.[['level']]) %>%
    subset(sapply(., nrow)>0) %>%
    lapply(st_union) %>%
    lapply(as.data.frame) %>% plyr::ldply(.id='level') %>%
    st_as_sf -> cpols3
  cpols3$level %>% as.character %>% as.numeric -> brks


  #subtract higher-level polygons from lower-level ones (transparency doesn't work if multiple polygons are overlaid
  donuts <- list()
  for(i in 1:(length(brks)-1)) {
    st_difference(cpols3 %>% subset(level==brks[i]),
                  cpols3 %>% subset(level==brks[i+1])) ->
      donuts[[i]]
  }

  #bind list items together
  donuts %>% do.call(rbind, .) %>% dplyr::select(-ends_with(".1")) -> donuts.all
  cpols3 %>% tail(1) %>% rbind(donuts.all, .) -> donuts.all

  #export
  donuts.all$level %<>% as.character() %>% as.numeric()
  outF <- paste0("SO2 donuts ",min(dates),"to",max(dates),"manmade")
  outF %>% paste0(".geojson") -> outF.gj
  outF.gj %T>% file.remove() %>% st_write(donuts.all, .)

  readLines(outF.gj) %>%
    (function(x) x[-grep('crs', x)[1]]) %>%
    writeLines(outF.gj)
}
