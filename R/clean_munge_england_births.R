
#' A Function to Clean and Merge Observed and Projected Births in England
#' @description This functions loads in observed and projected birth data produced by the Office
#'  of National Statistics and combines both datasets into a single tidy dataframe. Observed births can be
#'  downloaded 
#'  [here](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/vitalstatisticspopulationandhealthreferencetables),
#'   and projected births can be downloaded 
#'   [here](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/datasets/tablea14principalprojectionenglandsummary).
#' @inheritParams clean_demographics_uk
#' @param birth_path The file path to the unformated observed births data, see the description for details.
#' @param projected_birth_path The file path to the unformated projected births data, see the description for details.
#' @return A tidy data frame containing observed and projected births for England.
#' @export
#' @import magrittr
#' @import ggplot2
#' @importFrom readxl read_excel cell_cols cell_rows
#' @importFrom dplyr rename select mutate bind_rows
#' @examples
#'
#'
clean_and_munge_england_births <- function(birth_path = "~/data/tb_data/UK_demographics/annual_reference_table.xls",
                                          projected_birth_path = "~/data/tb_data/UK_demographics/england_population_projections.xls",
                                          return = TRUE,
                                          save = TRUE,
                                          save_name = "england_births",
                                          save_path = "~/data/tb_data/tbinenglanddataclean",
                                          save_format = c("rds", "csv"),
                                          verbose = TRUE,
                                          theme_set = theme_minimal) {
  if (is.null(birth_path)) {
    stop("The path to the observed birth data must be specified")
  }

  if (is.null(projected_birth_path)) {
    stop("The path to the projected birth data must be specified")
  }

  ## read in births
  obs_births <- read_excel(birth_path, sheet = "Births", range = cell_cols("A:D"), na = ":")

  ## Clean births and select english births only
  obs_births <- obs_births %>%
    na.omit %>%
    rename(year = Contents, births = X__3) %>%
    select(year, births) %>%
    mutate(year = as.numeric(year),
           births = as.numeric(births)) %>%
    mutate(data = "observed")

  ## manually clean years that are entered poorly
  obs_births <- obs_births %>%
    mutate(year = year %>%
             replace(year == 194010, 1940) %>%
             replace(year == 193910, 1939))

  ## read in projected_births
  proj_births <- read_excel(projected_birth_path,
                            sheet = "PERSONS",
                            range = cell_rows(c(6,10)))

  ## clean proj births - removing years that are present in the birth data
  proj_births <- proj_births[4,] %>%
    select(-X__1, -X__2) %>%
    gather(key = "year", value = "births") %>%
    mutate(births = as.numeric(births) * 1000) %>%
    mutate(year = as.numeric(year)) %>%
    filter(!(year %in% unique(obs_births$year))) %>%
    mutate(data = "projected")

  ## Join observed and projected birth data
  births <- obs_births %>%
    bind_rows(proj_births) %>%
    arrange(year) %>%
    mutate(data = as.factor(data))



  if (verbose) {
    ## graph to test data looks okay
    plot <- births %>%
      ggplot(aes(x = year, y = births, colour = data)) +
      geom_point(size = 0.5) +
      geom_line() +
      theme_set() +
      theme(legend.position = "bottom")

   plot
  }

  ## save data to repo and to data folder
  if (save) {
    save_data(births,
              name = save_name,
              path = save_path,
              format = save_format,
              message = "Demographic data saved to: ",
              verbose = verbose
    )
  }

  if (return) {
    return(births)
  }
}



