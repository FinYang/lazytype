#' @export
run_all_to_cursor <- function(){
  context <- rstudioapi::getActiveDocumentContext()
  end <- context$selection[[length(context$selection)]]$range$end[[1]]
  eval(parse(text = context$contents[1:end]), globalenv())
}
