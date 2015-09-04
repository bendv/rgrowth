#' @exportClass tsreg
#' @export

print.tsreg <- function(x) {
  if(x$start != x$disturbance)
    cat("\nStart of Monitoring Period: ", x$start)
  cat("\nInitial Disturbance:", x$disturbance)
  if(!is.na(x$regrowth_onset)) {
    cat("\nRegrowth onset:", x$regrowth_onset)
    cat("\nDuration (years) of post-regrowth stability:", x$s)
    cat("\nProportion of pre-regrowth MOSUM > critical boundary:", x$prereg_check)
  } else {
    cat("\nNo regrowth detected:", x$error_message)
  }
}