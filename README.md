
tbinenglanddataclean
====================

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/tbinenglanddataclean)](https://cran.r-project.org/package=tbinenglanddataclean) [![Build Status](https://travis-ci.org/seabbs/tbinenglanddataclean.svg?branch=master)](https://travis-ci.org/seabbs/tbinenglanddataclean) [![codecov](https://codecov.io/gh/seabbs/tbinenglanddataclean/branch/master/graph/badge.svg)](https://codecov.io/gh/seabbs/tbinenglanddataclean)

tbinenglanddataclean is an R package that contains functions and documentation to reproduce clean and munge available TB data in England.

Installation
------------

You can install tbinenglanddataclean from github with:

``` r
# install.packages("devtools")
devtools::install_github("seabbs/tbinenglanddataclean")
```

Raw data
--------

This package relies on raw data from several sources, these are;

1.  An extract of from the [Enhanced Tuberculosis Surveillance System](https://www.gov.uk/government/publications/tuberculosis-tb-in-england-surveillance-data). Access to this data requires an application to Public Health England.
2.  Demographic data from 2000, and from 2001 to 2015 from the [Office of National Statistics](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland) (ONS) this data can be downloaded freely.
3.  Data on births in the UK both observed and projected from the ONS, available [here](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/vitalstatisticspopulationandhealthreferencetables) and [here](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/vitalstatisticspopulationandhealthreferencetables).
4.  Data on age specific mortality rates from the ONS, available [here](%22https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/nationallifetablesenglandreferencetables%22).
5.  Survey information from the Labour Force Survey, as yearly extracts from 2000-2016 for the [April to June survey](https://discover.ukdataservice.ac.uk/catalogue/?sn=5461). Only registered users can download this data. Registration is possible for those at UK institutions. Other access arrangements can be made at request.

Cleaning and building the datasets
----------------------------------

The included vignette contains the code necessary to build all datasets associated with this package. Each function needs to be pointed at the correct raw data. If the default file names/locations are changed then this will also require updating. Contact [me](https://www.samabbott.co.uk) if you have any problems.

Other vignettes explore approaches for estimating demographic parameters from the clean and munged datasets.

### Docker

This packge was developed in a docker container based on the [tidyverse](https://hub.docker.com/r/rocker/tidyverse/) docker image. To run the docker image run:

``` bash
docker run -d -p 8787:8787 --name tbinenglanddataclean -e USER=seabbs -e PASSWORD=seabbs seabbs/tbinenglanddataclean
```

The rstudio client can be found on port `:8787` at your local machines ip. The default username:password is seabbs:seabbs, set the user with `-e USER=username`, and the password with `- e PASSWORD=newpasswordhere`. The default is to save the analysis files into the user directory.

To run a plain R terminal use:

``` bash
docker run --rm -it --user seabbs tbinenglanddataclean /usr/bin/R
```

To run a plain bash session:

``` bash
docker run --rm -it --user seabbs tbinenglanddataclean /bin/bash
```

To connect as root:

``` bash
docker exec -ti -u root tbinenglanddataclean bash
```
