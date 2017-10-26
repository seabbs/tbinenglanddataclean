## Start with the tidyverse docker image
FROM rocker/tidyverse:latest

MAINTAINER "Sam Abbott" contact@samabbott.co.uk

ADD . /home/seabbs

RUN Rscript -e 'devtools::install_dev_deps("/home/seabbs")'

RUN Rscript -e 'devtools::install_github("hadley/pkgdown")'

RUN Rscript -e 'devtools::install_github("tidyverse/ggplot2")'