#' Run Current Purl Section
#'
#' \code{run_current_purl_section} by its name, run the code in the purl section that the cursor is in.
#' I suggest binding this addin with shortcut \code{Shift+Alt+R}.
#' @author Yangzhuoran Yang
#' @seealso \code{run_selected_arguments}, \code{run_all_to_cursor}, \code{LazyScript}
#' @export
run_current_purl_section <- function(){
  context <- rstudioapi::getActiveDocumentContext()
  temp_script <- read_script(context$path, library = FALSE)
  lab <- "^(#|--)+\\s*(@knitr|----+)(.*?)-*\\s*$"
  temp <- rev(head(context$contents, context$selection[[1]]$range$start[[1]]))
  ind <- try(grep(lab, temp)[[1]], silent = TRUE)
  if(inherits(ind, "try-error")) stop("Cursor not in a purl section.")
  label <- stringr::str_trim(gsub(lab, "\\3", temp[[ind]]))
  temp_script$run(label, return_self = FALSE)
  # end <- context$selection[[length(context$selection)]]$range$end[[1]]
  # eval(parse(text = context$contents[1:end]), globalenv())
}




