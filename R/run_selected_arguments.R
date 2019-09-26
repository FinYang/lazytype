
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
      if(!identical((singleleft+1L), singleright))
        stop("Parentheses do not match. Check arguments in the highlighted area.")
      code[singleleft] <- paste(code[singleleft], code[singleright], sep = ",")
      code <- code[-singleright]
    }
  }
  eval(parse(text = code), globalenv())

  # code_run <- gsub(",", ";", code)
  # try(force(code_run))
}



