
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
    for(j in seq_len(ceiling(hours*60/5))){
      if(hours*60 <5) {
        Sys.sleep(hours*60*60)

        elap <- i*hours*60
        if(elap<60){
          cat("\r", paste("elapsed", round(elap, 2), "mins"))
        } else {
          cat("\r", paste("elapsed", floor(elap/60), "hour(s)", floor(elap%%60), "mins"))
        }
        break
      }
      Sys.sleep(60*5)
      elap <- j*5+(i-1)*ceiling(hours*60/5)*5
      if(elap<60){
        cat("\r", paste("elapsed", elap, "mins"))
      } else {
        cat("\r", paste("elapsed", floor(elap/60), "hour(s)", floor(elap%%60), "mins"))
      }
      utils::flush.console()
    }
    if(!is.null(beep))
      beepr::beep(beep)
    if(dialog){
      if(.Platform$OS.type=="windows"){
        if(i == ceiling(life/hours)) message <- paste0(message, "\n This is the last reminder.")
        utils::winDialog("ok", message)
      } else {
        warning("currently only support windows dialogue")
      }

    }
  }
}
