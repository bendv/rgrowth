#' @title Plot tsreg object
#' @description Plotting function for objects returned by \code{\link{tsreg}}
#' 
#' @param x Object of class ``tsreg"
#' @param ylabs Character. Vector of length 2 with the y-labels on the respective data and MOSUM plots
#' @param legend Logical. Include a legend in the plot?
#' 
#' @author Ben DeVries
#' 
#' @export

plot.tsreg <- function(x, ylabs = c("data", "|MOSUM|"), legend=TRUE) {

  # set up plotting area
  lo <- matrix(c(1:2), nr=2, nc=1)
  layout(lo)
  op <- par(mar = c(0, 5, 0, 5), oma = c(3, 3, 3, 3))
  
  # 1) top panel: data time series
  plot(x$data, xlab = '', xaxt = 'n', ylab = ylabs[1])
  lines(x$fit, col = 'blue')
  points(x$data[time(x$data) >= min(time(x$fit)) & time(x$data) <= max(time(x$fit))], type = 'p', pch = '*', cex = 0.7, col = 'blue')
  if(x$start != x$disturbance) {
    abline(v = as.numeric(x$disturbance), lty = 2, col = 'red')
    abline(v = as.numeric(x$start), lty = 2)
  } else {
    abline(v = as.numeric(x$start), lty = 2)
  }
  if(!is.na(x$regrowth_onset) & x$prereg_check > 0)
    abline(v = as.numeric(x$regrowth_onset), col = 'blue', lty = 3)
  
  # 2) bottom panel: MOSUM time series
  ymax <- max(abs(x$MOSUM), x$bound)
  plot(x$data, col = 'white', yaxt = 'n', ylim = c(min(abs(x$MOSUM)), ymax), ylab = ylabs[2])
  lines(abs(x$MOSUM), yaxt = 'n', lty = 2, col = 'blue')
  lines(zoo(x$bound, time(x$MOSUM)), col = 'green')
  if(x$start != x$disturbance) {
    abline(v = as.numeric(x$disturbance), lty = 2, col = 'red')
    abline(v = as.numeric(x$start), lty = 2)
  } else {
    abline(v = as.numeric(x$start), lty = 2)
  }
  if(!is.na(x$regrowth_onset) & x$prereg_check > 0)
    abline(v = as.numeric(x$regrowth_onset), col = 'blue', lty = 3)
  axis(4)
  
  # add legend
  if(legend) {
    ## TODO...
  }
    
  # reset graphical parameters
  layout(1)
  par(op)
}