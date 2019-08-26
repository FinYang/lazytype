#' @export
insert_scoping_assignment <- function(){
  context <- rstudioapi::getActiveDocumentContext()

  row_number <- context$selection[[1]]$range$start[1]
  column_length <- context$selection[[1]]$range$start[2]
  rstudioapi::insertText(c(row_number, column_length), text = " <<- ")

}
