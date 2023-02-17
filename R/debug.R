debug_env <- new.env(parent = emptyenv())

get_select_text <- function() {
  context <- rstudioapi::getActiveDocumentContext()
  if('#console' %in% context$id){
    message("Cursor at console")
    text <- context$contents
  } else {
    text <- context$selection[[1]]$text
  }
  text
}

#' @export
store_debug_expr <- function(expr = NULL){
  expr <- rlang::enexpr(expr)
  if(is.null(expr)){
    debug_env[["debug_expr"]] <- get_select_text()
  } else {
    debug_env[["debug_expr"]] <- deparse(expr)
  }
  message("Expression stored")
  invisible(NULL)
}

#' @export
rerun_debug <- function(){
  cat(debug_env[["debug_expr"]], sep = "\n")
  eval(parse(text = debug_env[["debug_expr"]]))
}
