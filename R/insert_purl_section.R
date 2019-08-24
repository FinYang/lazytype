
#' @export
insert_purl_section <- function(){
  context <- rstudioapi::getActiveDocumentContext()

  row_number <- context$selection[[1]]$range$start[1]
  column_length <- context$selection[[1]]$range$start[2]
  # r_end <- context$selection[[1]]$range$end[1]
  # r <- r_start:r_end
  line <- context$contents[row_number]
  is_purl <- substr(line, 1L, 7L) == "## ----"
  is_blank <- gsub(" ", "", line) == ""

  if(is_blank){
    dialogue <- getOption("purl_dialogue")
    if(is.null(dialogue) || dialogue){
      label <- winDialogString("Section label:", "")
      label_line <- paste0("## ---- ", label, " ----")
      rstudioapi::insertText(c(row_number, 1), text = paste0(label_line, "\n\n"))

    } else {
      rstudioapi::insertText(c(row_number, 1), text = "## ----  ----")
      rstudioapi::setCursorPosition(c(row_number, 9))

    }
  } else if(!is_purl){
    # not purl section
    label_line <- paste0("## ---- ", line, " ----")
    rstudioapi::modifyRange(c(row_number, 1, row_number, nchar(line)+1), text = paste0(label_line, "\n\n"))

  } else {
    newline <- sub("^## ----", "", line)
    if(substr(newline, 1L, 1L) == " ") newline <- substr(newline, 2, nchar(newline))
    newline <- gsub("----$", "", newline)
    if(substr(newline, nchar(newline), nchar(newline)) == " ") newline <- substr(newline, 1, nchar(newline)-1)
    rst/nudioapi::modifyRange(c(row_number, 1, row_number, nchar(line)+1), text = newline)
  }
}

# choose if using dialogue to insert puel section
#' @export
no_purl_dialogue <- function(nodialogue = TRUE){
  if(!nodialogue && .Platform$OS.type!="windows"){
    stop("lazytex: insert_purl_section currently only support windows dialogue")
  }
  options(purl_dialogue = !nodialogue)
}

#' @export
insert_rmarkdown_chunk_editlabel <- function(){
  context <- rstudioapi::getActiveDocumentContext()

  row_number <- context$selection[[1]]$range$start[1]
  line <- context$contents[row_number]
  is_blank <- gsub(" ", "", line) == ""
  while(is_blank != TRUE){
    row_number <- row_number +1
    if(row_number > length(context$contents) ){
      rstudioapi::insertText(c(row_number-1, nchar(line)+1), text = "\n")
      break
    }
    line <- context$contents[row_number]
    is_blank <- gsub(" ", "", line) == ""
  }
  rstudioapi::insertText(c(row_number, 1), text = "```{r }\n```\n")
  rstudioapi::setCursorPosition(c(row_number, 7))

}
