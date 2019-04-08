#' Clean and Munge UK Demographic Data
#'
#' @description A function that imports UK demographic data as two seperate files,
#' with demographics from 2001 to 2015 and demographics from 2000 only. Demographic data can be filtered
#' by country, and either saved to disk or returned to the R enviroment. Summary plots can be returned in
#' order to check the data. Data can be downloaded [here](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland)
#' @inheritParams clean_munge_ets_2016
#' @param data_path A charater string containing the file path to the demographic data.
#' @param demo_2000 A character string containing the file name of the demographic data for 2000.
#' @param demo_2001_2015 A character string containing the file name of the demographic data from 2001-2015
#' @param countries A character string, indicating with intials the countries to include in the
#' dataset. By default only England (E) is included.
#' @param save_name A character string contaning the file name for the data to be saved under.
#' @param save_path The filepath for the data to be saved in
#' @param theme_set The ggplot theme to apply to the summary graphs, defaults to theme_minimal
#' @return A tidy tibble of demographic data by age between 2000 and 2015 for the specified countries.
#' @export
#' @import magrittr
#' @importFrom readr read_csv
#' @importFrom dplyr mutate rename group_by summarise filter ungroup select 
#' @importFrom tidyr gather
#' @import ggplot2
#' @examples
#'
clean_demographics_uk <- function(data_path = "~/data/tb_data/UK_demographics",
                                  demo_2000 = "UK_2000_age.csv",
                                  demo_2001_2015 = "UK_2001_2015_age.csv",
                                  countries = c("E"),
                                  return = TRUE,
                                  save = TRUE,
                                  save_name = "E_demo_2000_2015",
                                  save_path = "~/data/tb_data/tbinenglanddataclean",
                                  save_format = "rds",
                                  verbose = TRUE,
                                  theme_set = theme_minimal) {

  ## demographics from 2001 to 2015
  demo_2001_2015 <- read_csv(file.path(data_path, demo_2001_2015))

## munge to EW, by age in tidy format - here age 90 is really a catch all category for all people aged 90 and over
demo_2001_2015 <- demo_2001_2015 %>%
  filter(country %in% countries) %>%
  group_by(Age) %>%
  summarise(`2001` = sum(population_2001),
            `2002` = sum(population_2002),
            `2003` = sum(population_2003),
            `2004` = sum(population_2004),
            `2005` = sum(population_2005),
            `2006` = sum(population_2006),
            `2007` = sum(population_2007),
            `2008` = sum(population_2008),
            `2009` = sum(population_2009),
            `2010` = sum(population_2010),
            `2011` = sum(population_2011),
            `2012` = sum(population_2012),
            `2013` = sum(population_2013),
            `2014` = sum(population_2014),
            `2015` = sum(population_2015)) %>%
  gather(key = Year, value = Population, 2:16) %>%
  mutate(Population = Population)

demo_2001_2015$Age <- factor(demo_2001_2015$Age)
demo_2001_2015$Year <- demo_2001_2015$Year %>% as.numeric
## demographics for 2000
demo_2000 <- read_csv(file.path(data_path, 'UK_2000_age.csv'))


## munge to same format as above
demo_2000 %>%
  select(Age, Persons) %>%
  rename(Population = Persons) %>%
  mutate(Year = rep(2000, nrow(demo_2000))) %>%
  filter(!(Age %in% 'All ages')) %>%
  mutate(Population = Population*1000) -> demo_2000

demo_2000$Age <- as.character(demo_2000$Age)

## join demographic data together
demo <- demo_2000 %>% bind_rows(demo_2001_2015)

## munge Age so that it is consistant
demo <- demo %>%
  mutate(Age = replace(Age, Age %in% c("90 and over", '90'), '90+'))

## Change class to make data vis easier
demo$Age <- factor(demo$Age, levels = c(as.character(0:89), '90+'))
demo$Year <- factor(demo$Year, levels = as.character(2000:2015))

demo  %>% filter(Year == '2001')

if (verbose) {
  ## graph to test data looks okay
  demo %>%
    ggplot(aes(x = Age, y = Population)) +
    geom_bar(stat = "identity") +
    facet_wrap(~Year) +
    theme_set() +
    theme(axis.text.x = element_text(angle = 45)) -> p

  p



  ## check a single year
  demo %>%
    filter(Year == "2015") %>%
    ggplot(aes(x = Age, y = Population)) +
    geom_bar(stat = "identity") +
    facet_wrap(~Year) +
    theme_set() +
    theme(axis.text.x = element_text(angle = 45)) -> p1

  p1
}

## save data to repo and to data folder
if (save) {
  save_data(demo,
            name = save_name,
            path = save_path,
            format = save_format,
            message = "Demographic data saved to: ",
            verbose = verbose
  )
}

if (return) {
  return(demo)
}

}
