# Test scripts voor uitrol nieuwe R en RStudio versies binnen INBO
## Test R script

-   Run the code in `test_script.R`
    -   Does it run without issues?

## Test R Markdown document

-   Knit the R Markdown document `test_script_rmd.Rmd`
    -   Does it run without issues?

## Test Quarto document

-   Render the Quarto document `test_script_qmd.qmd`
    -   Does it run without issues?

## Test `renv` project

-   Open `renv_project.Rproj` in folder `renv_project`
    -   Does it open RStudio without problems? (No crash)
    -   Read the `renv` startup message. Follow instructions.

-   Run the code in `script.R`
    -   Does it run without issues?

## Test INBOmd rapport

> more info: https://github.com/inbo/inbomd/

-   Open `inbo_rapport.Rproj` in folder `inbo_rapport`
-   Build the report (`INBOmd::gitbook`, `INBOmd::pdf_report`, `bookdown::word_document2`)
    -   Does it run without issues?
    
## Test flandersqmd rapport

> more info: https://github.com/inbo/flandersqmd/

-   Open `flandersqmd_rapport.Rproj` in folder `flandersqmd_rapport`
-   Render the report (`flandersqmd-book-html` format, `flandersqmd-book-pdf` format)
    -   Does it run without issues?
