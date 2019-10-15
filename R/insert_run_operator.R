#' Insert LazyScript Run Operator
#'
#' \code{insert_run_operator} insert \code{\%run\%} at cursor. Binding to \code{Alt+,} is suggested.
#'
#' @author Yangzhuoran Yang
#' @seealso \code{\%run\%}, \code{LazyScript}
#' @export
insert_run_operator <- function(){

  context <<- rstudioapi::getActiveDocumentContext()

  start <- c(context$selection[[1]]$range$start)
  end <- c(context$selection[[1]]$range$end)
  # rstudioapi::insertText(c(start, end), text = " <<- ")
  if(substr((context$contents[[start[[1]]]]), start[[2]]-1, start[[2]]-1)==" ")
    start[[2]] <- start[[2]]-1
  if(substr((context$contents[[end[[1]]]]), end[[2]], end[[2]])==" ")
    end[[2]] <- end[[2]]+1

  rstudioapi::modifyRange(c(start, end), text = " %run% ")

}
