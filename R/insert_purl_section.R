
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
      label_line <- paste0("## ---- ", label, " ",  strrep("-", 65-nchar(label)))
      rstudioapi::insertText(c(row_number, 1), text = paste0(label_line, "\n\n"))

    } else {
      rstudioapi::insertText(c(row_number, 1), text = "## --------")
      rstudioapi::setCursorPosition(c(row_number, 8))

    }
  } else if(is_purl){
    # is purl section
    if(nchar(line) < 75){
      rstudioapi::insertText(c(row_number, nchar(line)+1), text = paste0(strrep("-", 75-nchar(line)),  "\n"))
    }
  } else {
    #not purl section
    label_line <- paste0("## ---- ", line, " ",  strrep("-", 65-nchar(line)))
    rstudioapi::modifyRange(c(row_number, 1, row_number, nchar(line)+1), text = paste0(label_line, "\n\n"))
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
