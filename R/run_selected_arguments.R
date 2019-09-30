
#' @export
run_selected_arguments <- function(){
  context <- rstudioapi::getActiveDocumentContext()
  # print(context)
  text <- context$selection[[1]]$text
  # code <-
  code <- strsplit(gsub("\n", "", text), ",")[[1]]
  leftpa <- grepl("\\(", code)
  if(any(leftpa)){
    rightpa <- grepl("\\)", code)
    singlepa <- xor(leftpa, rightpa)
    singleleft <- which(singlepa & leftpa)
    if(length(singleleft)!=0){
      singleright <- which(singlepa & rightpa)
      if(length(singleleft) && length(singleright))
        stop("Parentheses do not match. Check arguments in the highlighted area.")
      para_poi <- mapply(function(a, b) a:b, a=singleleft, b=singleright)
      code_long <- sapply(para_poi, function(i) paste(code[i], collapse = ","))
      code_short <- code[-c(do.call(base::c, para_poi))]
      out_code <- c(code_long, code_short)
      # code[singleleft] <- paste(code[singleleft], code[singleright], sep = ",")
      # code <- code[-singleright]
    }
  }
  out_code <- out_code[-c(grep("=", out_code, invert = TRUE))]
  eval(parse(text = out_code), globalenv())

  # code_run <- gsub(",", ";", code)
  # try(force(code_run))
}



