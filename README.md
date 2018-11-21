# FFDB DLMTool shiny app

Presents various analyses of data stored in FFDB.

## Prerequisites

R and shiny server need to be installed, follow instructions at https://www.rstudio.com/products/shiny/download-server/

## Installation

Check out this repository, and install required R packages with:

    Rscript ./install-deps.R

...or study the script and perform equivalent steps.

Symlink this directory into the shiny server root, for example:

    ln -rs . /srv/shiny-server/ffdb-dlmtool

## Acknowledgements

This project has received funding from the European Union’s Horizon 2020 research and innovation programme under grant agreement no. 727891.
