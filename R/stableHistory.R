#' @title Stable history period selection
#' 
#' @description Determine stable history from a \code{bfastpp} data.frame. See \code{\link{bfastmonitor}} for more information.
#' 
#' @param data_tspp data.frame
#' @param start Numeric
#' @param formula
#' @param history
#' @param level
#' @param hpc
#' 
#' @details This function is used to determine the stable history in \code{\link{tsreg}}
#' 
#' @import strucchange
#' @seealso bfastmonitor
#' 
#' @author Jan Verbesselt, Achim Zeileis
#' 
#' @export

stableHistory <- function(data_tspp, start, formula, history, level, hpc) {
  
  ##################################
  ## Bai & Perron last breakpoint ##
  ##################################
  
  history_break <- function(formula, data, h = NULL, hpc = "none") {
    n <- nrow(data)
    ## rule of thumb for minimal segment size
    if(is.null(h)) h <- 6 * NCOL(model.matrix(formula, data = data[0,]))
    
    ## conduct breakpoints estimation
    bp <- breakpoints(formula, data = data, h = h, hpc = hpc)
    
    y_start <- tail(breakpoints(bp)$breakpoints, 1)
    y_start <- if(is.na(y_start)) 1 else y_start + 1
    data$time[y_start]
  }
  
  ########################################
  ## Reversely Ordered CUSUM (ROC) test ##
  ########################################
  
  ## A technique to verify whether or not the historical period is stable or not
  ## reversely order sample and perform
  ## recursive CUSUM test
  history_roc <- function(formula, data, level = 0.05) {
    n <- nrow(data)
    data_rev <- data[n:1,]
    data_rev$response <- ts(data_rev$response)
    y_rcus <- efp(formula, data = data_rev, type = "Rec-CUSUM")
    
    y_start <- if(sctest(y_rcus)$p.value < level) {
      length(y_rcus$process) - min(which(abs(y_rcus$process)[-1] > boundary(y_rcus)[-1])) + 1
    } else {
      1    
    }
    data$time[y_start]
  }
  
  level <- rep(level, length.out = 2)
  history_tspp <- subset(data_tspp, time < start)
  if (is.null(history)) {
    history <- start(history_tspp$response)
  }
  else if (all(is.character(history))) {
    #history <- match.arg(history)
    history <- switch(history, 
                      all = start(history_tspp$response), 
                      ROC = history_roc(formula, data = history_tspp, level = level[2]), 
                      BP = history_break(formula, data = history_tspp, hpc = hpc))
  }
  else if (all(is.function(history))) {
    history <- history(formula, data = history_tspp)
  }
  #history <- time2num(history)
  history_tspp <- subset(history_tspp, time >= history)
  
  return(history_tspp)
}