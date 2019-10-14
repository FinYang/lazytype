#' @export
LazyScript <- R6::R6Class("LazyScript", public = list(
  path = character(),
  script = NULL,
  initialize = function(path, library = TRUE){
    stopifnot(is.character(path))
    self$path <- path
    lines <- xfun::read_utf8(path)

    if (!length(lines)) {
      warning("code is empty")
    }
    lab <- knitr:::.sep.label

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

#' @export
read_script <- function(name, library = TRUE){
  LazyScript$new(name, library)
}


#' @export
`%run%` <- function(lazy_script, name){
  lazy_script$run(name)
}


#' @export
copy_script_to_rmd <- function(script_path, rmd_path = NULL, saveAll = TRUE, match_chunk = TRUE){
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
  rstudioapi::documentSaveAll()


  if(is.null(rmd_path)) rmd_path <- rstudioapi::getSourceEditorContext()$path
  lazy_script <- LazyScript$new(script_path, library = FALSE)
  text <- lazy_script$script
  text <- text[sapply(text, function(x) length(x)!=0)]

  if(match_chunk){
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
        warning("Chunk [", existing, "] have existing contents")
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



