
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
  run = function(name){
    eval(parse(text = self$script[[name]]))
    invisible(self)
  }

))

#' @export
read_script <- function(name, library = TRUE){
  LazyScript$new(name, library)
}


#' @export
`%run%` <- function(LazyScript, name){
  LazyScript$run(name)
}


