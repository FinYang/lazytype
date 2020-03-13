#' Run Selected Arguments
#'
#' \code{run_selected_arguments} runs selected code, ignoring the comma.
#' It normally is used to set the default arguments of a function where the arguments are separated by comma.
#' Useful for testing function with different arguments.
#' I suggest binding this addin with shortcut \code{Alt+R}
#'
#' @author Yangzhuoran Yang
#' @seealso \code{run_all_to_cursor}
#' @export
run_selected_arguments <- function(){
  context <- rstudioapi::getActiveDocumentContext()
  # context <<- rstudioapi::getActiveDocumentContext()
  # return(invisible("1"))
  if('#console' %in% context$id){
    message("Cursor at console")
    text <- context$contents
  } else {
    text <- context$selection[[1]]$text
  }
  # print(context)
  # code <-
  code <- strsplit(gsub("\n", "", text), ",")[[1]]
  leftpa <- grepl("\\(", code)
  if(any(leftpa)){
    rightpa <- grepl("\\)", code)
    singlepa <- xor(leftpa, rightpa) | (stringr::str_count(code, "(\\(|\\))") %% 2 !=0)
    singleleft <- which(singlepa & leftpa)
    if(length(singleleft)!=0){
      singleright <- which(singlepa & rightpa & (stringr::str_count(code, "\\)") >stringr::str_count(code, "\\(") ))
      csl <- stringr::str_count(code[singleleft], "\\(")
      crl <- stringr::str_count(code[singleright], "\\)")
      if(!sum(csl) && sum(crl))
        stop("Parentheses do not match. Check arguments in the highlighted area.")
      singlepa_i <- which(singlepa)

      met <- rbind(c(csl, crl)[order(c(singleleft, singleright))],
                   c(rep(1, length(singleleft)), rep(2, length(singleright)))[order(c(singleleft, singleright))],
                   singlepa_i)
      rownames(met) <- c("n_occurrence", "pa_type", "position")
      for(sp in singlepa_i){
        if(is.na(i <- match(sp, met[3,]))) next
        if(met[[2,i]]==1){
          cc <- met[[1,i]]
          for(s in seq(i+1, ncol(met))){
            if(met[[2,s]] == 1){
              cc <- cc + met[[1,s]]
            } else {
              cc <- cc - met[[1,s]]
            }
            if(cc==0){
              # rightpair[[match(sp, singlepa_i)]] <- met[3,s]
              # rightpair <- met
              if((i+1) !=s)
                met <- met[,-seq(i+1, s-1, by = 1)]
              break
            }
          }
        }
      }


      sl <- met[3,met[2,]==1]
      rl <- met[3,met[2,]==2]
      para_poi <- mapply(function(a, b) a:b, a=sl, b=rl, SIMPLIFY = FALSE)

      code_long <- sapply(para_poi, function(i) paste(code[i], collapse = ","))
      code_short <- code[-c(do.call(base::c, para_poi))]
      code <- c(code_long, code_short)
      # code[singleleft] <- paste(code[singleleft], code[singleright], sep = ",")
      # code <- code[-singleright]
    }
  }
  if(length(c(grep("=", code, invert = TRUE))) !=0)
    code <- code[-c(grep("=", code, invert = TRUE))]
  if(length(code) == 0)
    message("No assignment input")
  tmp <- utils::capture.output(eval(parse(text = code), globalenv()))
  # code_run <- gsub(",", ";", code)
  # try(force(code_run))
}



