# FFDB DLMTool shiny app

Presents various analyses of data stored in FFDB.

## Prerequisites

Firstly, R and required C libraries:

    apt install r-base \
        libxml2-dev libcurl4-openssl-dev zlib1g-dev libgit2-dev \
        libblas-dev liblapack-dev gfortran

...then install shiny server, follow instructions at https://www.rstudio.com/products/shiny/download-server/

## Installation

Check out this repository, and install required R packages with:

    Rscript ./install-deps.R

...or study the script and perform equivalent steps.

Symlink this directory into the shiny server root, for example:

    ln -rs . /srv/shiny-server/ffdb-dlmtool

## Configuration

At very least an empty ``local-config.js`` file is required:

    echo "" > local-config.js

Google analytics can be enabled by adding a tracking ID to ``local-config.js``:

    echo 'window.ga_code = "UA-12345-6";' > local-config.js

Set the tracking ID to ``UA-XXXXX-Y`` to debug the GA integration.

## Authors

* [Jamie Lentin](https://github.com/lentinj) - jamie.lentin@shuttlethread.com
* [Margarita Rincón Hidalgo](https://github.com/mmrinconh) - margarita.rincon@csic.es
* Javier Ruiz - javier.ruiz@csic.es

## License

This project is GPL-3.0 licensed - see the LICENSE file for details

## Acknowledgements

This project has received funding from the European Union’s Horizon 2020 research and innovation programme under grant agreement no. 727891.
