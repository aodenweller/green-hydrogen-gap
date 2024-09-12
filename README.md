# The Green Hydrogen Ambition and Implementation Gap

This respository contains the code to run the analysis and reproduce all figures of the article:

Odenweller, A., Ueckerdt, F.: *The green hydrogen ambition and implementation gap*

## System requirements

The R code in this repository has no specific system requirements. It should run on any system. Package dependencies are managed with `renv` (see Installation).

## Installation

For reproducibility, this repository uses [renv](https://rstudio.github.io/renv/articles/renv.html). In order to install all packages, in the main directory simply run:

```
renv::restore()
```

For some packages an additional repository has to be added in R:

```
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```

The [ggsankey](https://github.com/davidsjoberg/ggsankey) package may need to be installed manually from GitHub using `devtools`:

```
install.packages("devtools")
devtools::install_github("davidsjoberg/ggsankey")
```

## Instructions

Rendering the RMarkdown file `main.Rmd` will perform all analyses and reproduce all figures at once:

```
rmarkdown::render("main.Rmd")
```

Full execution should take no more than 5-10 minutes on a standard computer. Figures will be placed in the `figures` folder. Note that most figures were manually edited afterwards. Some data analysis statistics will be written to the output file, which defaults to `main.pdf`. 

The `data` folder contains all datasets required for the analysis. The `scripts` folder contains functions for data processing and plotting.
