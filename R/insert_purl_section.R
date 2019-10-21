#' Insert Purl Section
#'
#' \code{insert_purl_section} insert \code{## ----  ----} at an empty line in the script, or turns a non-empty line to a comment with purl section
#' header \code{## ----} (or turns it back). The header is recognised by \code{knitr::read_chunk} to read code for each chunk in the rmarkdown file from R script.
#' See \code{?knitr::read_chunk} for more details. I suggest binding this addin with shortcut \code{Shift+Ctrl+Q}.
#'
#' Windows user can trigger a dialogue when input label. Use \code{purl_dialogue(TRUE)} or set \code{options(purl_dialogue = TRUE)}
#' to turn it on.
#'
#'
#' @seealso \code{knitr::read_chunk}, \code{insert_rmarkdown_chunk_editlabel}
#' @author Yangzhuoran Yang
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
    if(!is.null(dialogue) || dialogue){
      label <- utils::winDialogString("Section label:", "")
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
    newline <- sub("^## ----* ", "", line)
    newline <- sub(" *---- *$", "", newline)
    rstudioapi::modifyRange(c(row_number, 1, row_number, nchar(line)+1), text = newline)
  }
}

#' @rdname insert_purl_section
#' @param dialogue Logical. Whether trigger a dialogue when input label.
#' @export
purl_dialogue <- function(dialogue = TRUE){
  if(dialogue && .Platform$OS.type!="windows"){
    stop("lazytex: insert_purl_section currently only support windows dialogue")
  }
  options(purl_dialogue = dialogue)
}

#' Insert Rmarkdown Chunk (Edit Label)
#'
#' \code{insert_rmarkdown_chunk_editlabel} insert Rmarkdown Chunk with cursor placed at the label.
#' Just a shortcut to insert chunk without any code in it to use with \code{knitr::read_chunk}.
#' I suggest binding this addin with shortcut \code{Ctrl+Alt+O}.
#'
#'
#' @seealso \code{knitr::read_chunk}, \code{insert_purl_section}
#' @author Yangzhuoran Yang
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
