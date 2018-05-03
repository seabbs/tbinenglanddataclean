
#' A Function to Clean and Munge Life Expectancy Estimates from England
#'
#' @description This functions loads in, cleans, and munges published mortality rates by age from the
#'  Office of National Statistics. The data required can be downloaded [here]("https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/nationallifetablesenglandreferencetables")
#' @inheritParams clean_and_munge_england_births
#' @param life_tables_path The file path to the unformated life tables data as a single xls sheet.
#' @param sheets A character vector of the names of the sheets to import, by default this will import all data 1999-2015.
#' @return A Tidy data frame containing age specific mortality rates for england between 1999 and 2015.
#' @export
#' @import magrittr
#' @import ggplot2
#' @importFrom readxl read_excel cell_cols
#' @importFrom dplyr rename select mutate bind_cols full_join mutate_at funs nth
#' @importFrom purrr map_df transpose
#' @importFrom stringr str_split
#' @importFrom ggridges geom_density_ridges
#' @examples
#'
clean_and_munge_england_life_expectancy <- function(life_tables_path =
                                                      "~/data/tb_data/UK_demographics/england_life_tables.xls",
                                                sheets = paste0(seq(2013,1998, -1), "-", seq(2015,2000, -1)),
                                                return = TRUE,
                                                save = TRUE,
                                                save_name = "england_mortality_rates",
                                                save_path = "~/data/tb_data/tbinenglanddataclean",
                                                save_format = c("rds", "csv"),
                                                verbose = TRUE,
                                                interactive = TRUE,
                                                theme_set = theme_minimal) {

  if (is.null(life_tables_path)) {
    stop("The path to the life tables data must be specified")
  }


  ## read in male mortality
  male_age_spec_mort <- sheets %>%
    map_df(~mutate(read_excel(life_tables_path, sheet = ., range = cell_cols("A:B"), na = ":"), year = .)) %>%
    mutate(gender = "male") %>%
    rename(age = `National Life Tables, England`,
           mortality_rate = X__1) %>%
    mutate_at(.vars = c("age", "mortality_rate"), .funs = funs(as.numeric(.))) %>%
    na.omit

  ## read in female mortality
  female_age_spec_mort <- sheets %>%
    map_df(~mutate(read_excel(life_tables_path, sheet = ., range = cell_cols("H"), na = ":"), year = .)) %>%
    mutate(Females = as.numeric(Females)) %>%
    rename(mortality_rate = Females) %>%
    na.omit %>%
    mutate(gender = "female")


  ## add ages to female mortality
  female_age_spec_mort <- bind_cols(select(male_age_spec_mort, age), female_age_spec_mort)

  ## bind the datasets together
  mortality_rates <- male_age_spec_mort %>%
    full_join(female_age_spec_mort, by = c("year", "gender", "age", "mortality_rate")) %>%
    mutate(mid_year = year %>% str_split("-") %>% transpose %>% nth(1) %>% unlist) %>%
    mutate(mid_year = as.numeric(mid_year) + 1) %>%
    select(year, mid_year, gender, age, mortality_rate) %>%
    mutate(gender = gender %>% factor(levels = c("male", "female")))

  if (verbose) {
    p1 <- mortality_rates %>%
      ggplot(aes(x = age , y = year, height = mortality_rate, fill = gender)) +
      geom_density_ridges(stat = "identity") +
      labs(caption = "Mortality rates over time, 3 year rolling estimates split by gender")

    interactive_plot(p1, FALSE)

    p2 <- mortality_rates %>%
      filter(age <= 65) %>%
      ggplot(aes(x = age , y = year, height = mortality_rate, fill = gender)) +
      geom_density_ridges(stat = "identity") +
      labs(caption = "Mortality rates over time, 3 year rolling estimates split by gender, for those 65 and under")

    interactive_plot(p2, FALSE)

    p3 <- mortality_rates %>%
      filter(age <= 5) %>%
      ggplot(aes(x = age , y = year, height = mortality_rate, fill = gender)) +
      geom_density_ridges(stat = "identity") +
      labs(caption = "Mortality rates over time, 3 year rolling estimates split by gender, for those 5 and under")

    interactive_plot(p3, FALSE)
  }
  ## save data to repo and to data folder
  if (save) {
    save_data(mortality_rates,
              name = save_name,
              path = save_path,
              format = save_format,
              message = "Mortality rates saved to: ",
              verbose = verbose
    )
  }

  if (return) {
    return(mortality_rates)
  }
}
