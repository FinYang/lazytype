#' Move mouse every 5 minutes to keep the PC awake
#'
#' Move the mouse around the current position (then go back to the original position)
#' every 5 minutes and print the time elapsed in the console.
#' \code{run_overnight} is just a wrapper of \code{dont_sign_me_out} for 24 hours.
#'
#' @param hours Numeric. The number of hours that you want to keep the PC awake.
#' @param reminder Numeric. Play a sound reminder every \code{reminder} hours if not \code{NULL}.
#' @param dialog Logical. Whether to show a dialog remainder if reminder is set.
#' @author Yangzhuoran Yang
#'
#'
#' @export
dont_sign_me_out <- function(hours = 3.5, reminder = NULL, dialog = TRUE){
  if(!is.null(reminder)){
    if(!requireNamespace("beepr"))
      stop("Need to install beepr to use the reminder functionality.")
  }
  test <- try(requireNamespace("rJava"))
  if("try-error" %in% class(test)) stop("You need to have Java installed.")
  if(!test) stop("You need to have package rJava installed.")

  hour <- 0
  day <- 0
  for(i in 1:ceiling(12*hours)){
    Sys.sleep(300)
    # if(i %% 2 == 0) rMouse::move(800,0) else rMouse::move(700,0)
    # rMouse::left()
    # posi <- (rMouse::coord())
    # posi <- try(rMouse::coord(), silent = TRUE)
    # if("try-error" %in% class(posi)){
    # rMouse:::.onLoad()
    jRobot <- rJava::.jnew("java/awt/Robot")
    jMouseInfo <- rJava::.jnew("java/awt/MouseInfo")
    x <- jMouseInfo$getPointerInfo()$getLocation()$x
    y <- jMouseInfo$getPointerInfo()$getLocation()$y
    # } else {
    #   x <- posi$x
    #   y <- posi$y
    # }
    jRobot$mouseMove(as.integer(x), as.integer(y+50))
    jRobot$mouseMove(as.integer(x+50), as.integer(y))
    jRobot$mouseMove(as.integer(x), as.integer(y-50))
    jRobot$mouseMove(as.integer(x-50), as.integer(y))
    jRobot$mouseMove(as.integer(x), as.integer(y))
    if(((i*5) %% 60) ==0)
      hour <- hour + 1
    if(((i*5) %% (60*24)) ==0)
      day <- day + 1
    if(day != 0){
      cat("\r",paste0(day, " day",ifelse(day ==1, " ", "s "), hour-24*day, " hour", ifelse(hour ==1, " ", "s ") , ifelse(i*5-60*hour >5, "", " "), i*5-60*hour, " mins"))
    } else if(hour != 0){
      cat("\r",paste0(hour, " hour", ifelse(hour ==1, " ", "s ") , ifelse(i*5-60*hour >5, "", " "),i*5-60*hour, " mins"))
    } else {
      cat("\r",paste0( ifelse(i >1, "", " "), i*5, " mins"))
    }
    utils::flush.console()
    if(!is.null(reminder)){
      if(i %% ceiling(12*reminder) ==0){
        beepr::beep()
      }
      if(dialog){
        if(.Platform$OS.type=="windows"){
          utils::winDialog("ok", "It's time to get up, drink some water and walk around.")
        } else {
          warning("currently only support windows dialogue")
        }

      }
    }
  }
}

#' @rdname dont_sign_me_out
#' @export
run_overnight <- function(){
  dont_sign_me_out(hours = 24)
}
