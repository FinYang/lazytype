#' @export
dont_sign_me_out <- function(hours = 3.5){
  hour <- 0
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
    if(hour != 0)
      cat("\r",paste0(hour, " hour", ifelse(hour ==1, " ", "s ") ,i*5-60*hour, " mins"))
    else
      cat("\r",paste0(i*5, " mins"))

    flush.console()
  }
}

#' @export
run_ovrenight <- function(){
  dont_sign_me_out(hours = 24)
}
