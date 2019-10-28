#' Interact between scripts, and rmarkdown files
#'
#' The \code{LazyScript} serie provides functions that can copy/run code between script and rmd files.
#' \code{LazyScript} is a R6 class, but the functions here provides means for user to use \code{LazyScript} instead of
#' handling the R6 system. See functions below for detailed usage. See \url{http://pkg.yangzhuoranyang.com/lazytype/} for more documentations.
#'
#' The \code{LazyScript} serie generally depends on the prul header \code{## ----} to seperate the sections in the script.
#' See \code{insert_purl_section} for addin to insert this header quickly. It is also the head that \code{knitr::read_chunk}
#' uses to regonise chunks.
#'
#'
#' @name LazyScript
#' @author Yangzhuoran Yang
#' @seealso \code{knitr::read_chunk}, \code{insert_purl_section}, \code{insert_run_operator}
#' @examples
#' \dontrun{
#' test_script <- read_script("test.R")
#'
#' test_script %run% "hello"
#'
#' copy_script_to_rmd("test.R", "test.Rmd")
#'
#' }
NULL

LazyScript <- R6::R6Class("LazyScript", public = list(
  path = character(),
  script = NULL,
  initialize = function(path, library = TRUE){
    stopifnot(is.character(path))
    if(gsub("^.*\\.(.*)$", "\\1", path) != "R") stop("Not R script.")
    self$path <- path
    lines <- xfun::read_utf8(path)

    if (!length(lines)) {
      warning("code is empty")
    }
    lab <- "^(#|--)+\\s*(@knitr|----+)(.*?)-*\\s*$"

    idx <- cumsum(grepl(lab, lines))
    if (idx[1] == 0) {
      idx = c(0, idx)
      lines = c("", lines)
    }
    groups <- unname(split(lines, idx))
    labels <- stringr::str_trim(gsub(lab, "\\3", sapply(groups, `[`, 1)))
    labels <- gsub(",.*", "", labels)
    code <- lapply(groups, knitr:::strip_chunk)
    for (i in which(!nzchar(labels))) labels[i] = paste0("unname_", i)
    self$script <- setNames(code, labels)
    if("library" %in% labels && library) self$run("library")
  },
  print = function(...){
    cat("LazyScript: \n")
    cat("  Path: ", self$path, "\n", sep = "")
    cat("  Number of section: ", length(self$script), "\n", sep = "")
    cat("  List of sections: ", paste(names(self$script), collapse = ", "), "\n", sep = "")
    invisible(self)
  },
  run = function(name, envir = globalenv()){
    if(!name %in% names(self$script)) stop("Chunk not found in the script.")
    eval(parse(text = self$script[[name]]), envir = envir)
    invisible(self)
  }

))

#' @param script_path String. The path to the script.
#' @param library Logical. Whether the library chunk (if there is one in the script) is ran when read in the script
#' @return A LazyScript object
#' @describeIn  LazyScript Read in script and store them in a LazyScript object.
#' Similar to \code{knitr::read_chunk} but in the context of script. Use purl header
#' \code{## ----} to set labels in the script.
#' @export
read_script <- function(script_path, library = TRUE){
  LazyScript$new(script_path, library)
}


#' @param LazyScript A LazyScript object.
#' @param chunk_name String. The label of the chunk in the script that you want to run.
#' @describeIn LazyScript Run the chunk in the LazyScript object based on the specified label.
#' Addin \code{insert_run_operator} can be used to insert it.
#' Note: use \code{print} explicitly in the script to print result in the console.
#' @export
`%run%` <- function(LazyScript, chunk_name){
  LazyScript$run(chunk_name)
}





#' @param rmd_path String. The path to the rmarkdown file. The default is the file currently opened in the Rstudio editor.
#' @param saveAll Logical. Whether save all open file in the editor. If FALSE, unsaved changes may be lost.
#' @param match_chunk Logical. If TRUE, the function tries to find chunks in the script matching the chunks in the rmd file and
#' make the copy to the corresponding chunks If FALSE, the chunks are appended to the end of the rmd file.
#' @param update Logical. If both \code{match_chunk} and \code{update} are TRUE, remove the existing contents in the chunk before copy-and-paste
#' @describeIn LazyScript Copy the code in the script to the rmarkdown file based on the chunk label (when \code{match_chunk = TRUE}),
#' or append the code to the end.
#' @export
copy_script_to_rmd <- function(script_path, rmd_path = NULL, saveAll = TRUE, match_chunk = TRUE, update = FALSE){
  # if(check){
  #   current_window <- rstudioapi::getSourceEditorContext()
  #   if(identical(normalizePath(rmd_path), normalizePath(current_window$path))){
  #     rmd_text <- xfun::read_utf8(rmd_path)
  #     if(!identical(current_window$contents, rmd_text))
  #       message("Unsaved change will be discarded")
  #     ans <- readline(prompt="Do you want to save unsaved content first? ([Y]es/[N]o/[C]ancel): ")
  #     if(ans=="Y"){
  #       rstudioapi::documentSave(current_window$id)
  #
  #     } else if(ans == "N"){
  #
  #     } else {
  #       message("Canceled")
  #       return()
  #     }
  #   } else if(saveAll){
  #     rstudioapi::documentSaveAll()
  #   }
  # } else if(saveAll){
  #   rstudioapi::documentSaveAll()
  # }
  if(saveAll)  rstudioapi::documentSaveAll()

  if(is.null(rmd_path)) rmd_path <- rstudioapi::getSourceEditorContext()$path
  lazy_script <- LazyScript$new(script_path, library = FALSE)
  text <- lazy_script$script
  text <- text[sapply(text, function(x) length(x)!=0)]


  if(match_chunk){
    if(update) remove_rmd_chunk(rmd_path)
    lines <- xfun::read_utf8(rmd_path)
    lab <- "^```\\{r (.*)\\}$"

    idx <- cumsum(grepl(lab, lines))
    groups <- unname(split(lines, idx))
    labels <- stringr::str_trim(gsub(lab, "\\1", sapply(groups, `[`, 1)))
    labels <- gsub(",.*", "", labels)
    extlab <- names(text)[names(text) %in% labels]
    if(!length(extlab)){
      message("No matching chunk found. Append script to the end.")
    } else {

      names(groups) <- labels
      code <- text[extlab]
      existing <- NULL
      for(lb in extlab){
        endpo <- grep("^```$", groups[[lb]])
        if(endpo>2) existing <- c(existing, lb)
        groups[[lb]] <- append(groups[[lb]], code[[lb]], endpo-1 )
      }
      added <- do.call(base::c, unname(groups))
      write(added, rmd_path)
      if(!is.null(existing)){
        warning("Chunk [", paste(existing, collapse = ","), "] have existing contents")
      }
      cat("Copy complete. Re-open rmd file if you don't see any changes.\n")
      return(invisible())
    }
  }


  start <-  paste0("\n```{r ",names(text) ,"}")
  chunk <- mapply(function(text, start) c(start, text, "```\n"),
                  start = start, text = text, SIMPLIFY = FALSE)
  chunk <- do.call(base::c, unname(chunk))
  write(chunk, rmd_path, append = TRUE)
  cat("Copy complete. Re-open rmd file if you don't see any changes.\n")
}

#' @param match_labels Vector of characters. The name of the chunk where contents to be removed.
#' If NULL, remove contents for all chunks except \code{setup}.
#' @describeIn LazyScript Remove the contents in the chunk.
#' @export
remove_rmd_chunk <- function(rmd_path, match_labels = NULL){
  if(!gsub("^.*\\.(.*)$", "\\1", rmd_path) %in% c("rmd", "Rmd")) stop("Not Rmarkdown file.")
  lines <- xfun::read_utf8(rmd_path)
  lab <- "^```\\{r (.*)\\}$"

  idx <- cumsum(grepl(lab, lines))
  groups <- unname(split(lines, idx))
  labels <- stringr::str_trim(gsub(lab, "\\1", sapply(groups, `[`, 1)))
  labels <- gsub(",.*", "", labels)
  names(groups) <- labels
  if(is.null(match_labels)) match_labels <- setdiff(labels,c("---", "setup"))
  for(lb in match_labels){
    endpo <- grep("^```$", groups[[lb]])
    groups[[lb]] <- groups[[lb]][-(2:(endpo-1))]
  }
  added <- do.call(base::c, unname(groups))
  write(added, rmd_path)
  return(invisible())
}

#' @describeIn LazyScript Wrapper of \code{copy_script_to_rmd} for \code{match_chunk = TRUE, update = FALSE}
#' @export
update_script_to_rmd <- function(script_path, rmd_path = NULL, saveAll = TRUE){
  copy_script_to_rmd(script_path, rmd_path, saveAll, TRUE, TRUE)
}


#
# LazyRmd <- R6::R6Class("LazyRmd", public = list(
#   path = character(),
#   rmd = NULL,
#   initialize = function(path, library = TRUE){
#     stopifnot(is.character(path))
#     if(!gsub("^.*\\.(.*)$", "\\1", path) %in% c("rmd", "Rmd")) stop("Not Rmarkdown file.")
#     self$path <- path
#     lines <- xfun::read_utf8(path)
#
#     if (!length(lines)) {
#       warning("file is empty")
#     }
#     lab <- "^```\\{r (.*)\\}$"
#
#     idx <- cumsum(grepl(lab, lines))
#     groups <- unname(split(lines, idx))
#     labels <- stringr::str_trim(gsub(lab, "\\1", sapply(groups, `[`, 1)))
#     labels <- gsub(",.*", "", labels)
#
#     names(groups) <- labels
#
#     self$rmd <- groups
#   },
#   print = function(...){
#     cat("LazyRmd: \n")
#     cat("  Path: ", self$path, "\n", sep = "")
#     cat("  Number of section: ", length(self$rmd), "\n", sep = "")
#     cat("  List of sections: ", paste(names(self$rmd), collapse = ", "), "\n", sep = "")
#     invisible(self)
#   }
# ))
