#' Run All up to Cursor
#'
#' \code{run_all_to_cursor} by its name, run the code in the current script above the cursor.
#' I suggest binding this addin with shortcut \code{Ctrl+Alt+R}.
#' @author Yangzhuoran Yang
#' @seealso \code{run_selected_arguments}, \code{run_current_purl_section}
#' @export
run_all_to_cursor <- function(){
  context <- rstudioapi::getActiveDocumentContext()
  end <- context$selection[[length(context$selection)]]$range$end[[1]]
  eval(parse(text = context$contents[1:end]), globalenv())
}
