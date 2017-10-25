## Start with the tidyverse docker image
FROM rocker/tidyverse:latest

MAINTAINER "Sam Abbott" contact@samabbott.co.uk

RUN installGithub.r hadley/pkgdown \
&& rm -rf /tmp/downloaded_packages/

ADD . /home/seabbs

RUN Rscript -e 'devtools::install_dev_deps("/home/seabbs")'

