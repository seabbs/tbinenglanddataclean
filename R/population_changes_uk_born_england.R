
#' A Function to Clean and Merge Observed and Projected Births in England
#' @description This functions loads in observed and projected birth data produced by the Office
#'  of National Statistics and combines both datasets into a single tidy dataframe. Observed births can be
#'  downloaded [here](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/vitalstatisticspopulationandhealthreferencetables),
#'   and projected births can be downloaded [here](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/datasets/tablea14principalprojectionenglandsummary).
#' @inheritParams clean_demographics_uk
#' @param birth_path The file path to the unformated observed births data, see the description for details.
#' @param projected_birth_path The file path to the unformated projected births data, see the description for details.
#' @return A tidy data frame containing observed and projected births for England.
#' @export
#' @import magrittr
#' @importFrom dplyr rename select mutate bind_rows
#' @examples
#'
#'
population_changes_uk_born_england <- function(birth_path = "~/data/tbinenglanddataclean/england_births.rds",
                                       demo_path = "~/data/tbinenglanddataclean/E_ons_lfs_2000_2016.rds",
                                       return = TRUE,
                                       save = TRUE,
                                       save_name = "population_changes",
                                       save_path = "~/data/tbinenglanddataclean",
                                       save_format = c("rds", "csv"),
                                       verbose = TRUE,
                                       interactive = TRUE,
                                       theme_set = theme_minimal) {

  if (is.null(birth_path)) {
    stop("The file path for english births data must be specified,
         run clean_munge_england_births to get the required dataset.")
  }

  if(is.null(demo_path)) {
    stop("The file path to the demographic data must be specified, see combine_ons_with_lfs
         for information about this datasdet")
  }

  ## read in data
  births <- readRDS(birth_path)
  demographics <- readRDS(demo_path)


  ## munge births
  births <- births %>%
    rename(Year = year, Births = births, Data = data) %>%
    mutate(Age = as.character(0))

  ## munge demographics
  demographics <- demographics %>%
    ungroup %>%
    mutate(Age = as.character(Age)) %>%
    select(-`Age group`, -`Age group (condensed)`) %>%
    filter(CoB %in% "UK born") %>%
    select(-CoB)

  ##Join dataframes
  demographics <- demographics %>%
    full_join(births, by = c("Year", "Age")) %>%
    mutate(Births = Births %>%
             replace(is.na(Births), 0)) %>%
    filter(Year >= 2000)

  ## Set nenonatal population equal to births if less than births

  ##Estimate yearly population changes that are not due to births
  demographics <- demographics %>%
    filter(Age) %>%
    mutate(mortality_emigration = lead(Population) - Population)

}
