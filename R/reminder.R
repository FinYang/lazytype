
#' Show reminder every specified hours
#'
#' @param hours Numeric. Play a sound reminder every \code{hours} hours.
#' @param message The message to show in dialog.
#' @param dialog Logical. Whether to show a dialog remainder if reminder is set. Only support Windows system.
#' @param beep Numeric. The number of the sound to beep in \code{beepr::beep}. Do not beep if \code{NULL}.
#' @param life Numeric. The number of hours that you want to keep the reminder runing (the reminder's life).
#' @author Yangzhuoran Yang
#'
#' @export
reminder <- function(hours = 1, message =  "It's time to get up, drink some water and walk around.", dialog = TRUE, beep = 1, life = 5){
  if(!is.null(reminder)){
    if(!requireNamespace("beepr"))
      stop("Need to install beepr to use the reminder functionality.")
  }
  for(i in 1:ceiling(life/hours)){
    Sys.sleep(hours*3600)
    cat("\r", paste("elapsed", i*hours, "hour(s)"))
    utils::flush.console()
    if(!is.null(beep))
        beepr::beep(beep)
      if(dialog){
        if(.Platform$OS.type=="windows"){
          utils::winDialog("ok", message)
        } else {
          warning("currently only support windows dialogue")
        }

      }
  }
}
