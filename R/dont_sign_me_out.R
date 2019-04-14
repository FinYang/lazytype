dont_sign_me_out <- function(hours = 3.5){
  for(i in 1:(12*hours)){
    Sys.sleep(300)
    if(i %% 2 == 0) rMouse::move(800,0) else rMouse::move(700,0)
    rMouse::left()
    print(paste(i*5, "mins"))
  }
}
