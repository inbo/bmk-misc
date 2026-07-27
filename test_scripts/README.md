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
    
## Test flandersqmd rapport

> Created with flandersqmd package v0.0.5
> More info: https://github.com/inbo/flandersqmd/

-   Open `flandersqmd_rapport.Rproj` in folder `flandersqmd_rapport`
-   Render the report (`flandersqmd-book-html` format, `flandersqmd-book-pdf` format)
    -   Does it run without issues?

## Test INBOmd rapport

> Created with INBOmd package v0.6.6
> More info: https://github.com/inbo/inbomd/

-   Open `inbo_rapport.Rproj` in folder `inbo_rapport`
-   Build the report (`INBOmd::gitbook`, `INBOmd::pdf_report`, `bookdown::word_document2`)
    -   Does it run without issues?
    
**Known warning messages:**

-   `INBOmd` is deprecated and will be no longer maintained after 2027. Switch to `flandersqmd` for longer support.
-   In `grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,`  :
  font family not found in Windows font database
-   In `grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,`  :
  font family not found in Windows font database
-   In `grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,`  :
  font family not found in Windows font database
-   In `grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,`  :
  font family not found in Windows font database
-   In `grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,`  :
  font family not found in Windows font database
-   In `grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,`  :
  font family not found in Windows font database
