new_lazyqmd <- function(x){
  stopifnot(inherits(x, "data.frame"))
  stopifnot(all(c("lines", "row", "code", "yaml", "title") %in% colnames(x)))
  `class<-`(x, c("lazyqmd", class(x)))
}

#' @export
lazyqmd <- function(path){
    stopifnot(tools::file_ext(path)=="qmd")
    lines <- xfun::read_utf8(path)

    idx_between_lab <- function(lab){
      grepl(lab, lines) %>%
        which() %>%
        matrix(ncol = 2, byrow = TRUE) %>%
        apply(1, function(range) do.call(seq, as.list(range))) %>%
        unlist()
    }
    code_idx <- idx_between_lab("^```.*$")
    yaml_idx <- idx_between_lab("^---$")
    qmd_file <- tibble(lines = lines) %>%
      mutate(row = row_number(),
             code = `[<-`(logical(length(lines)),code_idx, TRUE),
             yaml = `[<-`(logical(length(lines)),yaml_idx, TRUE),
             title = grepl("^ *#+ .*$", lines) & (!.data$code),
      )
    new_lazyqmd(qmd_file)
}
