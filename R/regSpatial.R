#' @title Monitor post-disturbance regrowth
#' @description Monitor post-disturbance regrowth using a LTS rasterBrick
#' 
#' @param x RasterBrick. Input time series brick.
#' @param change RasterLayer. Raster layer with inistial disturbance dates (e.g. breakpoints from \code{\link{bfastmonitor}}).
#' @param h MOSUM bandwidth (see \code{\link{tsreg}})
#' @param ... Additional arguments to be passed to \code{\link{tsreg}}
#' @param mc.cores Numeric. Number of cores for parallel processing (see \code{\link{mc.calc}})
#' @param filename Optional
#' 
#' @examples
#' ## ...coming soon...
#' 
#' @import bfastSpatial
#' @export
#' 
#' @author Ben DeVries
#' 
#' @seealso tsreg
#' 

regSpatial <- function(x, change, h=0.5, ..., filename = NULL, mc.cores = 1) {
  
  if(!hasArg(dates)) {
    s <- getSceneinfo(names(x))
    dates <- s$date
  }
  
  inp <- addLayer(change, x)
  
  fun <- function(z, ...) {
    bkp <- z[1]
    lts <- zoo(z[-1], dates)
    if(is.na(bkp)) {
      res <- rep(NA, 4)
    } else {
      err <- try(tsreg(x=lts, change=bkp, h=h, ...), silent = TRUE)
      if(class(err) == 'try-error') {
        res <- c(z[1], -9999, NA, NA)
      } else {
        res <- c(err$disturbance, err$regrowth_onset, err$s, err$prereg_check)
      }
    }
    return(res)
  }
  
  res <- mc.calc(inp, fun=fun, mc.cores = mc.cores)
  names(res) <- c('disturbance', 'regrowth_onset', 's', 'prereg_check')
  
  if(hasArg(filename))
    writeRaster(res, filename=filename)
  
  return(res)
}