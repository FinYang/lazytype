
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
    singlepa <- xor(leftpa, rightpa)
    singleleft <- which(singlepa & leftpa)
    if(length(singleleft)!=0){
      singleright <- which(singlepa & rightpa)
      if(!length(singleleft) && length(singleright))
        stop("Parentheses do not match. Check arguments in the highlighted area.")
      para_poi <- mapply(function(a, b) a:b, a=singleleft, b=singleright, SIMPLIFY = FALSE)
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
  tmp <- capture.output(eval(parse(text = code), globalenv()))
  # code_run <- gsub(",", ";", code)
  # try(force(code_run))
}



