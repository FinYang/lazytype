#' Add customised brackets around selection
#'
#' Add customised brackets around selection by setting addin using \code{bracket_selection}.
#' Set brackets and get currently set brackets.
#' Use two backslash "\\\\" when input backslash. See \code{Examples}.
#'
#' @author Yangzhuoran Yang
#' @export
bracket_selection <- function(){
  context <- rstudioapi::getActiveDocumentContext()

  row_number <- context$selection[[1]]$range$start[1]
  column_length <- context$selection[[1]]$range$start[2]
  brackets <- get_brackets()
  rstudioapi::modifyRange(c(context$selection[[1]]$range[[1]],
                            context$selection[[1]]$range[[2]]),
                          text = paste0(brackets[[1]],
                                        context$selection[[1]]$text,
                                        brackets[[2]]))
  if(context$selection[[1]]$text == ""){
    rstudioapi::setCursorPosition(c(row_number, context$selection[[1]]$range[[1]][[2]]+stringr::str_length(brackets[[1]])))
  }
}

#' @rdname bracket_selection
#' @param left left of the bracket
#' @param right right of the bracket
#' @examples
#' set_brackets(left = "\\boldsymbol{", right = "}")
#' @export
set_brackets <- function(left = "{", right = "}"){
  options(lazyBracket_left = left,
          lazyBracket_right = right)
  get_brackets()
}

#' @rdname bracket_selection
#' @return Return current brackets
#' @export
get_brackets <- function(){
  brackets <- (c(left = getOption("lazyBracket_left", "{"), right = getOption("lazyBracket_right", "}")))
  cat(paste0(brackets[["left"]], "example", brackets[["right"]], "\n\n"))
  return(invisible(brackets))

}


