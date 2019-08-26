#' @export
dont_sign_me_out <- function(hours = 3.5){
  for(i in 1:(12*hours)){
    Sys.sleep(300)
    if(i %% 2 == 0) rMouse::move(800,0) else rMouse::move(700,0)
    rMouse::left()
    # posi <- rMouse::coord()
    # x <- posi$x
    # y <- posi$y
    # move(x, y+50)
    # move(x+50, y)
    # move(x, y-50)
    # move(x-50, y)
    # move(x,y)
    cat("\r",paste(i*5, "mins"))
    flush.console()
  }
}

#' @export
run_ovrenight <- function(){
  dont_sign_me_out(hours = 24)
}
