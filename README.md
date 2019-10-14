
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lazytype

The R package *lazytype* provides addins for inserting code and running
code in a different manner, and functions to interact scripts with each
other and to interact script with rmarkdown file.

## Installation

The **stable** version on R CRAN is coming soon.

You can install the **development** version from
[Github](https://github.com/FinYang/lazytype) with:

``` r
# install.packages("devtools")
devtools::install_github("FinYang/lazytype")
```

## Usage

### Keep the PC awaken

`dont_sign_me_out(hours = 3.5)` moves mouse every 5 minutes for a
certain time (default 3.5 hours) and print the time elapsed in the
console. It is mainly used on public PC where it signs you out for a
certain period of inactivity.

### LazyScript

`LazyScript` operations helps with interacting with scripts and
rmarkdown files, saves time to copy and paste and provides means to
arrange code in a different way.

Assume in the current working directory there is a script file named
`test.R` with the following contents.

``` r
## ---- library ----
library(tidyverse)
## ---- hello ----
print("Hello World")
```

`read_script("test.R", library = TRUE)` reads in the code in the
`test.R` file and executes the chunk labelled *library* if argument has
been set to `library = TRUE`.

``` r
script_test <- read_script("test.R", library = TRUE)
```

`%run%` can be used to run a chunk from `test.R` file with a specified
label.

``` r
script_test %run% "hello"
```

    #> [1] "Hello World"

In other words, `read_script` provides a `knitr::read_chunk` equivalent
in script files.

`copy_script_to_rmd` copies code in the script to the rmd file in the
form of chunks based on the section header.

``` r
copy_script_to_rmd("test.R", "test.Rmd", match_chunk = FALSE)
```

In the `test.Rmd` file, after the existing text, the code in `test.R`
will be appended to the end in the following form.

![](man/figures/test.png)<!-- -->

### Addins

The package provides various addins, aiming to decrease keystrokes and
clicks.
<!-- A complete usage of addins please consult `vignette-addins` (coming soon). -->

#### Run All up to Cursor

`run_all_to_cursor` by its name, run the code in the current script
above the cursor. I suggest binding this addins with shortcut
`Ctrl`+`Alt`+`R`.

#### Run Selected Arguments

`run_selected_arguments` runs selected code, ignoring the comma. It
normally is used to set the default arguments of a function where the
arguments are separated by comma. Useful for testing function with
different arguments.

#### Insert Purl Section

`insert_purl_section` insert `## ---- ----` at an empty line in the
script, or turns a non-empty line to a comment with purl section header
`## ----`. The header is recognised by `knitr::read_chunk` to read code
for each chunk in the rmarkdown file from R script. See
`?knitr::read_chunk` for more details

#### Insert Rmarkdown Chunk (Edit Label)

`insert_rmarkdown_chunk_editlabel` insert Rmarkdown Chunk with cursor
placed at the label. Just a shortcut to insert chunk without any code in
it. Combined with `knitr::read_chunk`

#### Insert Scoping Assignment Operator

`insert_scoping_assignment` insert `<<-` at cursor. Binding to `Alt`+`=`
is suggested.

## License

This package is free and open source software, licensed under GPL-3.
