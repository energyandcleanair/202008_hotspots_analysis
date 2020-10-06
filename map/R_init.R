#add the following line to Rprofile.site in C:\Program Files\R\R-3.6.2\etc
#source("~/../Desktop/Box Sync/tools&templates/R_init.R")
#
# .lauR <- new.env()
#
 .lauR$llproj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
# .lauR$boxdir <- "~/../Desktop/My Drive/"
# .lauR$boxpath <- function(path) { paste0(boxdir,path) }
# .lauR$GISdir <- "~/../Desktop/My Drive/GIS/boundaries/"
# .lauR$GISpath <- function(path) { paste0(GISdir,path) }
# .lauR$tapmpath <- function(path="") { paste0(boxdir,"air pollution/TAPM/",path) }
# .lauR$'%eqna%' <- function(x, y) !is.na(x) & !is.na(y) & x==y
# .lauR$statmode <- function(x, na.rm = FALSE) {
#   if (na.rm) x <- x[!is.na(x)]
#   ux <- unique(x)
#   ux[which.max(table(match(x, ux)))]
# }
#
# .lauR$Dobson.unit = 2.69e16
#
#
# .lauR$'%whichin%' <- function(x,y) x[x %in% y]
# .lauR$'%notin%' <- function(x,y)!('%in%'(x,y))
# .lauR$'%whichnotin%' <- function(x,y) x[x %notin% y]
#
# .lauR$loadlibs <- function(general=defaults,
#                            plotting=defaults,
#                            geo=defaults,
#                            xls=defaults,
#                            geocoding=defaults,
#                            defaults=T,
#                            custom=NULL,
#                            install.missing=T,
#                            check.updates=T,
#                            online=F,
#                            crea=T) {
#   if(is.null(online))
#     online <- (system2("ping", "baidu.com", stderr = FALSE, stdout = FALSE) == 0)
#
#   reqlibs <- custom
#   if(general)
#     reqlibs <- c(reqlibs, "magrittr","plyr","tidyverse",
#                  "reshape2","data.table","lubridate","zoo",
#                  "pbapply")
#
#   if(plotting)
#     reqlibs <- c(reqlibs,"lattice","RColorBrewer","ggplot2","ggthemes", "ggsci")
#
#   if(geo)
#     reqlibs <- c(reqlibs,"sp","raster","rasterVis","rworldmap","rgdal","sf","geosphere","gstat", "rgeos")
#
#   if(xls) reqlibs <- c(reqlibs,"readxl")
#
#   if(geocoding) reqlibs <- c(reqlibs,"ggmap")
#
#   unique(reqlibs) -> reqlibs
#
#   if(check.updates & online) {
#     if(nrow(old.packages()) > 0)
#       readline('packages need to be updated, continue y/n?') -> r
#
#     if(tolower(r) == 'y')
#       update.packages(ask=F)
#   }
#
#   inst.libs <- row.names(installed.packages())
#   needed.libs <- reqlibs[!(reqlibs %in% inst.libs)]
#
#   if(install.missing & online & length(needed.libs) > 0) {
#     install.packages(needed.libs)
#   } else {
#     if(length(needed.libs)>0) warning(paste(paste(needed.libs,sep=','),'not installed!'))
#     reqlibs <- reqlibs[reqlibs %in% inst.libs]
#   }
#
#   lapply(reqlibs, library, character.only = T)
#   if(geocoding) register_google("AIzaSyAQlelAGN3MIbHZyCWF3ujAj5yEAEEl80s")
#
#   if(crea) source(boxpath('CREA/CREAtheme.R'))
# }
#
#
.lauR$spdf <- function(data,crs=NULL,llcols=NULL,na.action=na.omit) {
  if(grepl('^Spatial',class(data))) {
    warning('Data is already of type Spatial*')
    return(data)
  }

  if(class(data) != 'data.frame')
    as.data.frame(data) -> data

  require(sp)

  if(is.null(llcols)) {
    llcols <- unlist(sapply(c("^longitude","^latitude"),grep,tolower(names(data))))

    if(length(llcols)!=2)
      llcols <- unlist(sapply(c("^lon","^lat"),grep,tolower(names(data))))

    if(length(llcols)!=2)
      llcols <- unlist(sapply(c("x","y"),function(str) { which(str == tolower(names(data))) }))

    if(length(llcols)!=2)
      llcols <- unlist(sapply(c("^x","^y"),grep,tolower(names(data))))

    if(length(llcols)<2)
      stop("could not identify coordinate columns, need to start with lat, lon or x, y")

    if(length(llcols)>2)
      stop("could not identify coordinate columns, too many starting with lat, lon or x, y")
  }

  if(anyNA(data[,llcols])) warning("NAs in coordinates")
  data[row.names(na.action(data[,llcols])),] -> data

  if(is.null(crs)) {
    crs <- llproj
    warning("assuming lat-lon WGS84 coordinate system")
  }

  if(class(crs) == "character")
    crs <- CRS(crs)

  return(SpatialPointsDataFrame(coords = data[,llcols],data=data,proj4string = crs))
}
#
# .lauR$geocode.loop <- function(locs,repeats=10, source=c("google", "dsk"), ...) {
#   require(ggmap)
#
#   if(locs %>% dim %>% length <= 1) {
#     data.frame(name=locs,lon=NA,lat=NA,stringsAsFactors = F) -> locs
#   } else {
#     locs <- as.data.frame(locs)
#     if(is.null(locs$lat)) locs$lat <- NA
#     if(is.null(locs$lon)) locs$lon <- NA
#   }
#
#   counter=1
#   while(anyNA(locs$lat) & counter <= repeats * length(source)) {
#     paste("geocoding",sum(is.na(locs$lat)),"locations") %>% print
#     locs[is.na(locs$lat),c('lon','lat')] <-
#       geocode(locs[is.na(locs$lat),'name'],
#               source=source[ceiling(counter / repeats)], ...)
#     counter = counter + 1
#   }
#   return(locs)
# }
#
# .lauR$getdatelist <- function(dateinput,year=NULL) {
#   require(lubridate)
#   seq(as.Date(dateinput[1]),as.Date(last(dateinput)),by='day') -> dateoutput
#   if(!is.null(year))
#     lapply(year,
#            function(year) {
#              year(dateoutput) <- year
#              return(dateoutput)
#            }) %>% do.call(c, .) -> dateoutput
#   return(dateoutput)
# }
#
#
# .lauR$getUTMproj <- function(zone=NULL,hem=NULL,loc=NULL,units="km") {
#
#   if(!is.null(loc) & (!is.null(zone) | !is.null(hem)))
#     warning("using explicit zone / hemisphere settings to override coordinate input")
#
#   if(!is.null(loc) & grepl("Spatial",class(loc))) {
#     require(sp)
#     if(proj4string(loc) != llproj) loc <- spTransform(loc,CRS(llproj))
#     ll <- colMeans(coordinates(loc))
#   }
#
#   if(!is.null(loc) & !grepl("Spatial",class(loc))) {
#     warning("numeric location input - assuming lon-lat coordinate system")
#     ll <- loc
#   }
#
#   if(is.null(zone))
#     zone <- floor((ll[1] + 180)/6) %% 60 + 1
#
#   if(is.null(hem)) {
#     southhemi <- ll[2] < 0
#   } else southhemi <- tolower(substr(hem,1,1)) == "s"
#
#
#
#   paste0("+proj=utm +datum=WGS84 +no_defs +zone=",zone,ifelse(southhemi," +south ","")," +units=",units)
# }
#
# .lauR$sigfloor <- function(x,sigdig=1) {
#   mag <- 10^floor(log10(x)-sigdig+1)
#   return(floor(x/mag)*mag)
# }
#
# .lauR$cleanup <- function(protect,threshold=1) {
#   sort( sapply(ls(envir = .GlobalEnv),function(x){object.size(get(x))}),decreasing = T)/1e6 -> memsizes
#   trash <- rep(F,length(memsizes))
#
#   for(o in which(memsizes>threshold)) {
#     print(paste0("total memory size: ",round(sum(memsizes[!trash]),1),"Mb"))
#     print(paste0(ifelse(o==1,"","next "),"largest object: ",names(memsizes)[o],", ",round(memsizes[o],1),"Mb"))
#     readline("remove y/n/q ") -> reply
#     if(substr(reply,1,1) %in% c("", "q")) break()
#
#     if(substr(reply,1,1) == "y") trash[o] <- T
#   }
#
#   if(sum(trash==T)==0) return()
#
#   rmObjs <- names(memsizes[trash])
#   print(paste("removing",paste(rmObjs,collapse=" ")))
#   readline("continue y/n ") -> reply2
#   #rmObjs <- names(memsizes[memsizes>10e6 & !(names(memsizes) %in% c(protectObjects,"total"))])
#   if(substr(reply2,1,1) == "y") rm(list=rmObjs, envir = .GlobalEnv)
# }
#
# #show all pch symbols
# .lauR$pchcheat <- function() {
#   plot(x=rep(1:5,5),y=sapply(1:5,rep,5),pch=1:25)
#   text(x=rep(1:5,5),y=sapply(1:5,rep,5),1:25,pos=c(rep(3,5),rep(1,20)))
# }
#
# #show all named colors
# .lauR$colcheat <- function(cl=colors(), bg = "grey",
#                      cex = 0.75, rot = 30) {
#   m <- ceiling(sqrt(n <-length(cl)))
#   length(cl) <- m*m; cm <- matrix(cl, m)
#   require("grid")
#   grid.newpage(); vp <- viewport(w = .92, h = .92)
#   grid.rect(gp=gpar(fill=bg))
#   grid.text(cm, x = col(cm)/m, y = rev(row(cm))/m, rot = rot,
#             vp=vp, gp=gpar(cex = cex, col = cm,font=2))
# }
#
# #helper function to normalize values to 100 in a specified column (e.g. date in a 'wide format' df)
# .lauR$norm100 <- function(df,cols,basedate) {
#   if(is.null(df$date)) stop('date column not present')
#   df[,cols] <-
#     lapply(df[,cols],function(x) {
#       x.base = x[which(df$date == basedate[1])]
#       if(!is.finite(x.base)) x.base <- x[which(df$date == basedate[2])]
#       x / x.base  * 100
#     } )
#   return(df)
# }
#
# .lauR$norm.0 <- function(df,cols,basedate) {
#   if(is.null(df$date)) stop('date column not present')
#   df[,cols] <-
#     lapply(df[,cols],function(x) {
#       x.base = x[which(df$date == basedate[1])]
#       if(!is.finite(x.base)) x.base <- x[which(df$date == basedate[2])]
#       x - x.base
#     } )
#   return(df)
# }
#
# .lauR$capitalize.first <- function(x, rest_lower=T) {
#   tolower_if = function(x) {if(rest_lower) {return(tolower(x))}else return(x)}
#   sapply(x, function(x) {
#     s <- strsplit(x, " ")[[1]]
#     paste(toupper(substring(s, 1,1)), tolower_if(substring(s, 2)),
#         sep="", collapse=" ")
#   } ) -> x.clean
#   return(x.clean)
# }
#
# .lauR$try.download <- function(urls,
#                                destfiles=gsub('.*/','',urls),
#                                tries=10,mode='wb',
#                                overwrite=T,
#                                quiet=T,
#                                verbose=T,...) {
#
#   outputs <- data.frame(url=urls,file=destfiles,status='missing',stringsAsFactors = F)
#
#   if(!overwrite)
#     outputs$status <- ifelse(file.exists(destfiles),'already on disk','missing')
#
#   if(file.exists('try.download.temp')) file.remove('try.download.temp')
#   for(i in 1:nrow(outputs)) {
#
#     if(outputs[i,'status'] == 'missing') {
#       tries <- 0
#       while(!file.exists('try.download.temp') & tries < 10) {
#         try(download.file(outputs[i,'url'],destfile = 'try.download.temp',
#                           quiet=quiet,mode=mode, ...))
#         tries = tries+1
#       }
#
#       if(file.exists('try.download.temp')) {
#         file.rename('try.download.temp',outputs[i,'file'])
#         outputs[i,'status'] <- 'downloaded'
#       }
#     }
#
#     if(verbose)
#       print(paste('file',outputs[i,'file'],outputs[i,'status'],
#                   paste0('(',i,' of ',nrow(outputs),')')))
#   }
#
#   return(outputs)
# }
#
# .lauR$is.outlier <- function(x, SDs=10,na.rm=F) {
#   abs((x - mean(x, na.rm=na.rm)) / sd(x, na.rm = na.rm)) -> devs
#   return(is.na(devs) | devs > SDs)
# }
#
# .lauR$latticeTheme <- function(col=brewer.pal(9,'Spectral'),reso=1) {
#   parSets <- simpleTheme(col=cols)
#   trellis.par.get() -> tp
#   trellis.par.set(parSets)
#   trellis.par.get() -> parSets
#   trellis.par.set(tp)
#
#   #line widths
#   lwd.def=1.5*reso
#   parSets$superpose.line$lwd=lwd.def
#   parSets$superpose.polygon$lwd=lwd.def
#   parSets$plot.line$lwd=lwd.def
#   parSets$plot.polygon$lwd=lwd.def
#   parSets$axis.line$lwd=lwd.def
#
#   #plot symbol sizes
#   parSets$superpose.symbol$cex=reso
#   parSets$plot.symbol$cex=reso
#
#   #text sizes
#   parSets$par.main.text$cex=1
#   parSets$add.text$cex=1
#   parSets$fontsize$text=12*reso
#   parSets$add.text$lineheight = 1.2 + reso/5
#
#   return(parSets)
# }
#
# #calculate mean with maximum amount of NA values specified
# .lauR$mean.maxna <- function(x,maxna) {
#   if(sum(is.na(x))>maxna) { return(as.numeric(NA))
#   } else return(mean(x,na.rm=T))
# }
#
# .lauR$col.a <- function(colorname,alpha) {
#   colorname %>% col2rgb %>% unlist %>% divide_by(255) -> cn
#   rgb(cn[1],cn[2],cn[3],alpha)
# }
#
# .lauR$poscheat <- function() {
#   c(-1:1) %>% data.frame(lat=., lon=.,z=.) %>% spdf -> t1
#   spplot(t1,zcol='z') +
#     layer(sp.text(coordinates(t1[c(2,2,2,2),]),
#                   txt=as.character(1:4), pos=1:4))
# }
#
# .lauR$darjeeling <- function(ordering=T,...) {
#   scale_color_manual(values=wesanderson::wes_palette("Darjeeling1")[ordering],...)
# }
#
# .lauR$quickpng <- function(file, width=2000, height=1500, res=300, ...) {
#   png(filename=file, width=width, height=height, res=res, ...)
# }
#
# .lauR$dms_to_dec <- function(x) {
#   x %>%
#     lapply(function(x) {
#       xn = x %>% strsplit('[^0-9.,]+') %>% unlist %>% gsub(',', '.', .) %>% as.numeric
#       if(is.na(xn[3])) xn[3] <- 0
#       x_dec = xn[1]+xn[2]/60+xn[3]/3600
#       if(grepl("S$|W$", x)) x_dec %<>% multiply_by(-1)
#       return(x_dec)
#     }) %>%
#     unlist
# }
#
# .lauR$getadm <- function(level=0, version='36') {
#   require(sp)
#   inF <- GISpath(paste0('gadm',version,'_',level,'.RDS'))
#
#   if(file.exists(inF)) {
#     readRDS(inF)
#   } else {
#     raster::shapefile(gsub('\\.RDS','.shp',inF),
#                       encoding='UTF-8', use_iconv=TRUE)
#   }
# }
#
# .lauR$getpopdens <- function() { raster(GISpath('gpw-v4-2015-density/gpw-v4-population-density-adjusted-to-2015-unwpp-country-totals_2015.tif')) }
# .lauR$getpopcount <- function() { raster(GISpath('gpw-v4-population-count-2015/gpw-v4-population-count_2015.tif')) }
#
# .lauR$paste.xl <- function(header=T, ...)
#   read.table('clipboard', sep='\t', header=header, ...)
# .lauR$copy.xl <- function(df, col.names=T, quote=F, row.names=F, ...)
#   write.table(df, 'clipboard', sep='\t',
#               col.names=col.names, quote=quote, row.names=row.names, ...)
#
# .lauR$showPNG <- function(f) grid::grid.raster(png::readPNG(f))
# .lauR$sel <- dplyr::select
#
# .lauR$col.gradient <- function(colors=c('white', 'gray', 'yellow', 'orange',
#                            'red','darkred', 'black'),
#                          n=40, alpha=1,
#                          ...) {
#   if(length(alpha)==2) alpha=seq(alpha[1], alpha[2], length.out=length(colors))
#
#   colors %>% col2rgb %>% t %>%
#     rgb(alpha=alpha*255, maxColorValue = 255) -> colors
#
#
#   colorRamp(colors, alpha=!is.null(alpha),...)(seq(0,1, length.out = n)) %>%
#     rgb(alpha=.[,4], maxColorValue = 255)
# }
#
# .lauR$orderfactor <- function(var, by) {
#   var = factor(var, levels = var[rev(order(by))])
# }
#
# .lauR$setyear <- function(x, year=2020) {lubridate::year(x) <- year; x}
#
# .lauR$countrycrop <- function(x, country, expansion=0, ...) {
#   rworldmap::countriesLow %>%
#     subset(SOVEREIGNT == country) -> bb_shape
#   if(grepl("Spatial|Raster", class(x))) {
#     bb_shape %>% extent %>% add(expansion) %>%
#       crop(x, ., ...) -> r.out
#   }
#
#   if(grepl("sf", class(x))) {
#     bb_shape %>% st_bbox %>%
#       add(expansion * c(-1, -1, 1, 1)) %>%
#       st_crop(x, ., ...) -> r.out
#   }
#   return(r.out)
# }
#
# .lauR$cropProj <- function(shapeobj, rasterobj, expand=4, ...) {
#   shapeobj %>%
#     crop(extent(projectExtent(rasterobj,
#                               crs(shapeobj)))+expand) -> shapeobj
#   if(grepl("Raster", class(shapeobj))) {
#     shapeobj %>% projectRaster(rasterobj, ...) %>% return
#   } else shapeobj %>% spTransform(crs(rasterobj)) %>% return
# }
#
# .lauR$getpop <- function(what='density') {
#   if(what=='density') f = GISpath('gpw-v4-2015-density/gpw-v4-population-density-adjusted-to-2015-unwpp-country-totals_2015.tif')
#   if(what=='count') f = GISpath('gpw-v4-population-count-2015/gpw-v4-population-count_2015.tif')
#   raster(f)
# }
#
# .lauR$getPM25 <- function() raster(boxpath('GIS/GlobalAmbientPM25/GlobalGWRcwUni_PM25_GL_201601_201612-RH35_Median.nc'))
#
# #cluster points
# .lauR$cluster <- function(sp, distKM) {
#   require(geosphere)
#   sp <- spdf(sp)
#   hc <- sp %>% coordinates %>% distm %>% as.dist %>% hclust
#   cutree(hc,h=distKM*1000)
# }
#
# .lauR$recurse <- function (L, f, targetclass="data.frame",...) {
#   if(inherits(L, targetclass) | !is.list(L)) f(L, ...)
# }
#
#
#
attach(.lauR)
# print('custom init file loaded')
