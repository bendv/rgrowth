#' @title Monitor post-disturbance regrowth
#' @description Structural monitoring method for post-disturbance forest regrowth in a time series
#' 
#' @param x Time series. Can be an object of type \code{numeric} (vector), \code{zoo} or \code{ts}
#' @param change Numeric. Date of original disturbance. See \code{\link{bfastmonitor}} for a potential method to determine this on a time series.
#' @param h Numeric. Bandwidth of the MOSUM monitoring window. See \code{\link{bfastmonitor}} for more information.
#' @param dates Date. Vector with length \code{length(x)} with observation dates. If omitted, \code{x} will simply be an ordered series with arbitrary (regularly spaced) dates
#' @param type Character. Type of time series. Can be either "irregular" (include gaps) or "16-day" (ie. MODIS-type)
#' @param startOffset Numeric. Number of (decimal) years by which to offset the start of the regrowth monitoring process. Can also be set to "floor", in which case the \code{floor} of the change date will be taken as the start of the monitoring period.
#' @param formula Formula to be fit to stable history period (see \code{\link{bfastmonitor}})
#' @param order Numeric. Order of the harmonic term of \code{formula} (see \code{\link{bfastmonitor}})
#' @param history Method or pre-determined period to use for history model (see \code{\link{bfastmonitor}})
#' @param level Numeric. See \code{\link{bfastmonitor}}
#' @param w Numeric. Number of years before a regrowth flag is allowed to be assigned.
#' @param s Numeric. Number of years in which MOSUM values must be below the critical threshold for a regrowth label to be assigned.
#' @param plot Logical. Plot the results?
#' @param ylabs Character. Vector of length 2 with y labels of the data time series and MOSUM plots, respectively 
#' 
#' @return object of class \code{tsreg}
#' 
#' @import bfast
#' @import strucchange
#' @import zoo
#' @export
#' 
#' @author Ben DeVries
#' 
#' @seealso regSpatial
#' 
#' @examples
#' data(ndmi)
#' plot(ndmi, type='b', cex=0.5)
#' 
#' # breakpoint detection with bfastmonitor
#' x <- bfastts(ndmi, time(ndmi), type="irregular")
#' bfm <- bfastmonitor(x, start=c(2005, 1), formula=response~harmon, order=1, plot=TRUE)
#' 
#' reg <- tsreg(ndmi, change = bfm$breakpoint, h = 0.5, plot = TRUE)
#' print(reg)
#' 
#' reg2 <- tsreg(ndmi, change = bfm$breakpoint, startOffset = "floor", 
#' h = 0.5, plot = TRUE)
#' print(reg2)
#' 
#' reg3 <- tsreg(ndmi, change = bfm$breakpoint, startOffset = "floor", 
#' h = 0.5, history='all', plot=TRUE)
#' print(reg3)
#' 
#' reg4 <- tsreg(ndmi, change = bfm$breakpoint, startOffset = "floor", 
#' h = 0.5, history='all', s=0, plot=TRUE)
#' print(reg4)

tsreg <- function(x, change, h, dates = NULL, type = c("irregular", "16-day"), startOffset = 0, formula=response~harmon, order=1, history='BP', level=0.05, w = 3, s = 1, plot = FALSE, ylabs = c('data', '|MOSUM|')) {
  
  # make a regularlized bfastts
  type <- type[1]
  if(is.null(dates)) {
    z <- bfastts(x, dates = time(x), type = type)
  } else {
    z <- bfastts(x, dates, type = type)
  }
  
  # error message (to be included in output)
  err <- NULL
  
  # set start period
  if(startOffset == "floor"){
    start <- floor(change)
  } else {
    start <- change - startOffset
  }
  
  # derive history model parameters
  df <- bfastpp(z, order = order)
  history_pp <- stableHistory(df, start=start, formula=formula, history=history, level=level, hpc="none")
  mod <- lm(formula=formula, data=history_pp)
  
  # predict values for all times
  df$prediction <- predict(mod, newdata=df)
  df$prediction[df$time < min(history_pp$time)] <- NA
  df$residual <- df$response - df$prediction
  
  # initiate mefp with historical data
  y <- ts(df$residual[which(df$time < start)])
  m <- mefp(y ~ 1, type = "OLS-MOSUM", h = h)
  
  # add all data
  y <- ts(df$residual)
  m <- monitor(m, verbose = FALSE)
  
  # just take lowest bound for now (this can change over time)
  bound <- min(boundary(m))
  
  # subset df$time to overlap with abs(m$process)
  mostime <- df$time[which(df$time >= start)]
  
  ### conditions for reg to be assigned:
  # 1) (tR - tB) >= w
  suppressWarnings(reg1 <- min(mostime[which(mostime >= change + w & abs(m$process) < bound)]))
  if(reg1 == Inf | is.na(reg1) | length(reg1) == 0) {
    reg1 <- NA
    err <- "no regrowth detected"
  }
  
  # 2) stable period after tR >= S
  ## for how long do MOSUMS stay under bound?
  ## choose the first 'stable' segment with duration > s
  ### TODO: can we speed this up by combining this with condition (1)?
  if(!is.na(reg1)){
    
    ## get time segments where abs(m$process) < bound
    st <- abs(m$process[which(mostime >= reg1)]) < bound
    
    if(!all(st) & !all(!st) & length(st) > 0) {
      seg <- lapply(rle(st)$lengths, seq_len)
      for(i in 2:length(seg))
        seg[[i]] <- seg[[i]] + seg[[i-1]][length(seg[[i-1]])]
      segt <- lapply(seg, FUN=function(x) mostime[mostime > reg1][x])
      segdur <- sapply(segt, FUN=function(x) x[length(x)] - x[1])
      finalseg <- min(which(segdur > s & rle(st)$values))
      ## TODO: fix bug in above line:
        ## tsreg(ndmi, change=2005, h=0.25)
        ## Warning message:
          ##In min(which(segdur > s & rle(st)$values)) :
          ##no non-missing arguments to min; returning Inf
      
      if(finalseg != Inf) {
        reg2 <- segdur[finalseg]
        reg1 <- min(segt[[finalseg]]) ## revised reg1
      } else {
        reg2 <- NA
        err <- "stability parameter (s) not computed"
      }
    } else if(all(!st)) {
      reg2 <- NA
      err <- "no regrowth detected (2)"
    } else {
      sttime <- mostime[which(mostime > reg1)]
      reg2 <- sttime[length(sttime)] - sttime[1]
    }
  } else {
    reg2 <- NA
  }
  
  if(!is.na(reg2) & reg2 < s) {
    reg1 <- reg2 <- NA
    err <- "stability criterion not met"
  }
  
  # 3) at least some MOSUMs between tB and (tB + tR) are greater than bound
  if(!is.na(reg1)){
    reg3 <- sum(abs(m$process[which(mostime > change & mostime < reg1)]) > bound) / length(m$process[which(mostime > change & mostime < reg1)])
    # expressed as a proportion of total observations in that time
    ## FIX ME: use w (fails if set to 0) or reg1
  } else {
    reg3 <- NA
  }
  
  # object to return
  res <- list(start=start,
              disturbance=change,
              regrowth_onset=reg1, 
              s=reg2, 
              prereg_check=reg3, 
              data=zoo(z[!is.na(z)], time(z)[!is.na(z)]),
              fit=zoo(df$prediction[!is.na(df$prediction)], df$time[!is.na(df$prediction)]),
              MOSUM=zoo(m$process, mostime),
              bound=bound,
              error_message=err)
  
  class(res) <- "tsreg"
  
  # plot
  if(plot) 
    plot(res)
  
  return(res)
}
