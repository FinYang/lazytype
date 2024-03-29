% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LazyScript.R
\name{LazyScript}
\alias{LazyScript}
\alias{read_script}
\alias{\%run\%}
\alias{copy_script_to_rmd}
\alias{remove_rmd_chunk}
\alias{remove_qmd_chunk}
\alias{lift_inchunk_label}
\alias{update_script_to_rmd}
\title{Interact between scripts, and rmarkdown files}
\usage{
read_script(script_path, library = TRUE)

LazyScript \%run\% chunk_name

copy_script_to_rmd(
  script_path,
  rmd_path = NULL,
  saveAll = TRUE,
  match_chunk = TRUE,
  update = FALSE
)

remove_rmd_chunk(rmd_path, match_labels = NULL)

remove_qmd_chunk(qmd_path, match_labels = NULL)

lift_inchunk_label(md_path, inchunk_rm = FALSE)

update_script_to_rmd(script_path, rmd_path = NULL, saveAll = TRUE)
}
\arguments{
\item{script_path}{String. The path to the script.}

\item{library}{Logical. Whether the library chunk (if there is one in the script) is ran when read in the script}

\item{LazyScript}{A LazyScript object.}

\item{chunk_name}{String. The label of the chunk in the script that you want to run.}

\item{rmd_path}{String. The path to the rmarkdown file. The default for \code{copy_script_to_rmd} is the file currently opened in the Rstudio editor.}

\item{saveAll}{Logical. Whether save all open file in the editor. If FALSE, unsaved changes may be lost.}

\item{match_chunk}{Logical. If TRUE, the function tries to find chunks in the script matching the chunks in the rmd file and
make the copy to the corresponding chunks If FALSE, the chunks are appended to the end of the rmd file.}

\item{update}{Logical. If both \code{match_chunk} and \code{update} are TRUE, remove the existing contents in the chunk before copy-and-paste}

\item{match_labels}{Vector of characters. The name of the chunk where contents to be removed.
If NULL, remove contents for all chunks except \code{setup}.}

\item{qmd_path}{String. The path to the Quarto file.}

\item{md_path}{String. The path to the rmarkdown or quarto file.}

\item{inchunk_rm}{Logical. Whether to remove the in-chunk label line once it is moved to the chunk header}
}
\value{
A LazyScript object
}
\description{
The \code{LazyScript} serie provides functions that can copy/run code between script and rmd files.
\code{LazyScript} is a R6 class, but the functions here provides means for user to use \code{LazyScript} instead of
handling the R6 system. See functions below for detailed usage. See \url{http://pkg.yangzhuoranyang.com/lazytype/} for more documentations.
}
\details{
The \code{LazyScript} serie generally depends on the prul header \code{## ----} to seperate the sections in the script.
See \code{insert_purl_section} for addin to insert this header quickly. It is also the head that \code{knitr::read_chunk}
uses to regonise chunks.
}
\section{Functions}{
\itemize{
\item \code{read_script()}: Read in script and store them in a LazyScript object.
Similar to \code{knitr::read_chunk} but in the context of script. Use purl header
\code{## ----} to set labels in the script.

\item \code{LazyScript \%run\% chunk_name}: Run the chunk in the LazyScript object based on the specified label.
Addin \code{insert_run_operator} can be used to insert it.
Note: use \code{print} explicitly in the script to print result in the console.

\item \code{copy_script_to_rmd()}: Copy the code in the script to the rmarkdown file based on the chunk label (when \code{match_chunk = TRUE}),
or append the code to the end.

\item \code{remove_rmd_chunk()}: Remove code chunk content in rmd files.

\item \code{remove_qmd_chunk()}: Remove code chunk content in qmd files.

\item \code{lift_inchunk_label()}: Put in-chunk labels to chunk headers

\item \code{update_script_to_rmd()}: Wrapper of \code{copy_script_to_rmd} for \code{match_chunk = TRUE, update = FALSE}

}}
\examples{
\dontrun{
test_script <- read_script("test.R")

test_script \%run\% "hello"

copy_script_to_rmd("test.R", "test.Rmd")

}
}
\seealso{
\code{knitr::read_chunk}, \code{insert_purl_section}, \code{insert_run_operator}
}
\author{
Yangzhuoran Yang
}
