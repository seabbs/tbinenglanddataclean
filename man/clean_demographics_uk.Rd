% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_demographics_uk.R
\name{clean_demographics_uk}
\alias{clean_demographics_uk}
\title{Clean and Munge UK Demographic Data}
\usage{
clean_demographics_uk(data_path = "~/data/tb_data/UK_demographics",
  demo_2000 = "UK_2000_age.csv",
  demo_2001_2015 = "UK_2001_2015_age.csv", countries = c("E"),
  return = TRUE, save = TRUE, save_name = "E_demo_2000_2015",
  save_path = "~/data/tb_data/tbinenglanddataclean",
  save_format = "rds", verbose = TRUE, theme_set = theme_minimal)
}
\arguments{
\item{data_path}{A charater string containing the file path to the demographic data.}

\item{demo_2000}{A character string containing the file name of the demographic data for 2000.}

\item{demo_2001_2015}{A character string containing the file name of the demographic data from 2001-2015}

\item{countries}{A character string, indicating with intials the countries to include in the
dataset. By default only England (E) is included.}

\item{return}{Logical, defaults to \code{TRUE}. Specifies whether to return cleanded data.}

\item{save}{Logical, defaults to \code{FALSE}. Specifies whether to save data}

\item{save_name}{A character string contaning the file name for the data to be saved under.}

\item{save_path}{The filepath for the data to be saved in}

\item{save_format}{A character vector specifying the format/formats to save the data into, defaults to rds. Currently
csv is also supported. See \code{\link[idmodelr]{save_data}} for details.}

\item{verbose}{A logical indicating whether summary information should be provided.}

\item{theme_set}{The ggplot theme to apply to the summary graphs, defaults to theme_minimal}
}
\value{
A tidy tibble of demographic data by age between 2000 and 2015 for the specified countries.
}
\description{
A function that imports UK demographic data as two seperate files,
with demographics from 2001 to 2015 and demographics from 2000 only. Demographic data can be filtered
by country, and either saved to disk or returned to the R enviroment. Summary plots can be returned in
order to check the data. Data can be downloaded \href{https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland}{here}
}
\examples{

}
