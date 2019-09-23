#' @export
dont_sign_me_out <- function(hours = 3.5){
  hour <- 0
  day <- 0
  for(i in 1:(12*hours)){
    Sys.sleep(300)
    # if(i %% 2 == 0) rMouse::move(800,0) else rMouse::move(700,0)
    # rMouse::left()
    # posi <- (rMouse::coord())
    posi <- try(rMouse::coord(), silent = TRUE)
    if("try-error" %in% class(posi)){
      rMouse:::.onLoad()
      posi <- rMouse::coord()
    }
    x <- posi$x
    y <- posi$y
    rMouse::move(x, y+50, failSafe = FALSE)
    rMouse::move(x+50, y, failSafe = FALSE)
    rMouse::move(x, y-50, failSafe = FALSE)
    rMouse::move(x-50, y, failSafe = FALSE)
    rMouse::move(x,y, failSafe = FALSE)
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
    flush.console()
  }
}

#' @export
run_ovrenight <- function(){
  dont_sign_me_out(hours = 24)
}
