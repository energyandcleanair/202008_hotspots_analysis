mean.loop <- function(r, filename=rasterTmpFile()) {
  bs <- blockSize(r)
  r.out <- raster(r)
  r.out <- writeStart(r.out, filename=filename, overwrite=TRUE)
  for(i in 1:bs$n) {
    getValues(r, bs$row[i], bs$nrows[i]) -> r.sub
    rowMeans(r.sub, na.rm=T) -> r.sub  
    r.out <- writeValues(r.out, r.sub, bs$row[i])
    cat('\rprocessed chunk ', i,' out of ', bs$n)
  }
  r.out <- writeStop(r.out)
  return(r.out)
}

focal.loop <- function(r, w, fun, filename=rasterTmpFile(), ...) {
  require(compiler)
  require(magrittr)
  
  #compile in hopes of speeding up
  cmp_fun <- cmpfun(fun)
  
  bs <- blockSize(r)
  
  #adjust chunksize down to account for extra "padding" rows
  bs <- blockSize(r, chunksize = ncol(r) * (bs$nrows[1]-dim(w)[1]))
  
  #initialize
  r.out <- raster(r)
  r.out <- writeStart(r.out, filename=filename, overwrite=TRUE)
  pad.rows = (dim(w)[2]-1)/2 #number of rows to add above and below
  for(i in 1:bs$n) {
    #calculate number of padding rows, avoid trying to read outside raster
    padUp = min(c(bs$row[i]-1, pad.rows))
    padDown = min(c(nrow(r) - bs$row[i] - bs$nrows[i] + 1, pad.rows))
    
    #read a chunk of rows and convert to matrix
    getValues(r, bs$row[i] - padUp,
              bs$nrows[i] + padUp + padDown) %>% 
      matrix(ncol=ncol(r), byrow=T) -> r.sub
    
    #convert to raster, run focal and convert to vector for writing
    r.sub %>% raster %>% 
      focal(w=w,fun=cmp_fun, ...) %>% 
      matrix(ncol=ncol(r), byrow=T) -> r.sub
    r.sub[(padUp+1):(nrow(r.sub)-padDown),] %>% 
      t %>% as.vector-> r.sub
    
    #write the rows back
    r.out <- writeValues(r.out, r.sub, bs$row[i])
    cat('\rprocessed chunk ', i,' out of ', bs$n)
  }
  #save and exit
  r.out <- writeStop(r.out)
  return(r.out)
}

raster.loop <- function(r, fun, filename=rasterTmpFile(), ...) {
  bs <- blockSize(r)
  r.out <- raster(r)
  r.out <- writeStart(r.out, filename=filename, overwrite=TRUE)
  for(i in 1:bs$n) {
    getValues(r, bs$row[i], bs$nrows[i]) -> r.sub
    apply(r.sub, 1, fun, ...) -> r.sub  
    r.out <- writeValues(r.out, r.sub, bs$row[i])
    cat('\rprocessed chunk ', i,' out of ', bs$n)
  }
  r.out <- writeStop(r.out)
  return(r.out)
}

zonal.loop <- function(x, z, fun, ...) {
  bs <- blockSize(r)

  for(i in 1:bs$n) {
    getValues(r, bs$row[i], bs$nrows[i]) -> r.sub
    getValues(z, bs$row[i], bs$nrows[i]) -> z.sub
    apply(r.sub, 2, function(x) aggregate(x, by=list(z.sub), FUN=fun, na.rm=T)) -> r.agg
    fun(r.sub, ...) -> r.sub  
    r.out <- writeValues(r.out, r.sub, bs$row[i])
    cat('\rprocessed chunk ', i,' out of ', bs$n)
  }
  r.out <- writeStop(r.out)
  return(r.out)
}


focalcircle <- function(r, d, 
                        fun=function(x) weighted.mean(x, fw.c, na.rm=T),
                        ...) {
  fw.r=focalWeight(r, d=d, type="rectangle")
  fw.c=focalWeight(r, d=d, type="circle")
  fw.r[,] <- 1
  fw.c[fw.c>0] <- 1
  focal.loop(r, w=fw.r, pad=T, fun=fun, ...)
}

