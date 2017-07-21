#' Function to Calculate Incidence Rates from ETS and Demographic Data
#'
#' @description This function returns multiple incidence rate datesets, with varying age
#' groupings. This enables more rapid interrogation of the data.
#' @inherit combine_ons_with_lfs
#' @param ets_name A character string of the name for the cleaned ETS data.
#' @param demo_name A character string of the name for the cleanded
#' demographic ONS and LFS data.
#' @param incidence_name A character string indicating the name to save the incidence data under.
#' @param grouped_incidence_name A character string indicating the name to save the age grouped
#' incidence data under.
#' @param condensed_grouped_incidence_name A character string indicating the name to save the condensed age
#' grouped data under.
#' @param cases_demo_incidence_name A character string indicating the name to save the cases, and demographic data under.
#'
#' @return A list of dataframes containing incidence data in an increasingly condensed format.
#' @export
#' @import magrittr
#' @import dplyr
#' @importFrom tidyr gather spread
#' @examples
#'
calculate_incidence_ets_lfs_ons <- function(data_path = "~/data/tbinenglanddataclean",
                                 ets_name = "clean_ets_2016.rds",
                                 demo_name = "E_ons_lfs_2000_2016.rds",
                                 return = TRUE,
                                 save = TRUE,
                                 incidence_name = "incidence" ,
                                 grouped_incidence_name = "age_grouped_incidence",
                                 condensed_grouped_incidence_name = "condensed_age_group_incidence",
                                 cases_demo_incidence_name = "cases_demo_incidence",
                                 save_path = "~/data/tbinenglanddataclean",
                                 save_format = "rds",
                                 verbose = TRUE,
                                 interactive = TRUE,
                                 theme_set = theme_minimal) {
  ## Load ETS data
  ets_path <- file.path(data_path, ets_name)
  if (verbose) {
    message("Loading ets data from: ", ets_path)
  }
  ets <- readRDS(ets_path)

  ## Load Demographic data
  demo_path <- file.path(data_path, demo_name)
  if (verbose) {
    message("Loading demographic data from: ", demo_path)
  }
  demo <- readRDS(demo_path)

  ## Extract cases by birth status from the ETS - set those above 90 into a 90+ category to account for low numbers
  case_year_age <- ets %>% extract_case_counts


  ## Add total cases rows
  total_case_year_age <- case_year_age %>%
    group_by(Age, Year, `Age group`, `Age group (condensed)`) %>%
    summarise(Total = sum(Cases), `Total (LFS)` = sum(Cases))

  ## Bind into dataframe
  case_year_age <-  case_year_age %>%
    spread(key = CoB, value = Cases) %>%
    full_join(total_case_year_age, by = c('Age', 'Year', 'Age group', 'Age group (condensed)')) %>%
    gather(key = CoB, value = Cases, Total, `Total (LFS)`, `UK Born`, `Non-UK Born`, `<NA>`) %>%
    mutate(CoB = recode_factor(CoB, `UK Born` = 'UK born', `Non-UK Born` = 'Non-UK born')) %>%
    mutate(CoB = replace(CoB, CoB %in% '<NA>', NA) %>% droplevels)

  # Join data sets ----------------------------------------------------------
  cases_demo <- demo %>%
    full_join(case_year_age, by = c('Year', 'Age', 'CoB', 'Age group', 'Age group (condensed)')) %>%
    mutate(Cases = replace(Cases, is.na(Cases),0)) %>%
    mutate(CoB = factor(CoB))


  # Add Total for all ages --------------------------------------------------]

  cases_demo %>%
    filter(!is.na(Age)) %>%
    select(-Cases, -`Age group`, -`Age group (condensed)` ) %>%
    spread(key = Age, value = Population) -> pop_age

  cases_demo %>%
    filter(!(is.na(Age))) %>%
    select(-`Age group`, -`Age group (condensed)`,  -Population) %>%
    spread(key = Age, value = Cases) -> cases_age


  incidence  <- unique(cases_demo$CoB) %>%
    map(function(filter) est_inc_crude_adj(filter,
                                           pop = pop_age,
                                           cases = cases_age,
                                           rate_pop = 100000)) %>%
    bind_rows %>%
    rename(Age = cov) %>%
    mutate(Age = Age %>%
             factor(levels = c( 'All cases (crude)', 'All cases (adj)',
                              as.character(0:89), '90+')))


  # Estimate incidence for age groupings ------------------------------------
  cases_demo_age_grouped <- cases_demo %>%
    ungroup  %>%
    group_by(`Age group`, Year, CoB) %>%
    summarise(Cases = sum(Cases), Population = sum(Population)) %>%
    ungroup



  cases_demo_age_grouped %>%
    filter(!is.na(`Age group`)) %>%
    select(-Cases) %>%
    spread(key = `Age group`, value = Population) -> pop_age

  cases_demo_age_grouped %>%
    filter(!(is.na(`Age group`))) %>%
    select(-Population) %>%
    spread(key = `Age group`, value = Cases) -> cases_age


  age_grouped_incidence  <- unique(cases_demo_age_grouped$CoB) %>%
    map(function(filter) est_inc_crude_adj(filter, pop = pop_age,
                                           cases = cases_age,
                                           rate_pop = 100000)) %>%
    bind_rows %>%
    rename(`Age group` = cov) %>%
    mutate(`Age group` =  `Age group` %>%
             factor(levels = c( 'All cases (crude)', 'All cases (adj)',
                                paste(seq(0,85,5), seq(4,89,5), sep = '-'), '90+')))

  # Estimate incidence for  young adult, working age, 65+ age groupings------------------------
  cases_demo_age_grouped_condensed <- cases_demo %>%
    ungroup  %>%
    group_by(`Age group (condensed)`, Year, CoB) %>%
    summarise(Cases = sum(Cases), Population = sum(Population)) %>%
    ungroup



  cases_demo_age_grouped_condensed %>%
    filter(!is.na(`Age group (condensed)`)) %>%
    select(-Cases) %>%
    spread(key = `Age group (condensed)`, value = Population) -> pop_age

  cases_demo_age_grouped_condensed %>%
    filter(!(is.na(`Age group (condensed)`))) %>%
    select(-Population) %>%
    spread(key = `Age group (condensed)`, value = Cases) -> cases_age


  age_grouped_condensed_incidence  <- unique(cases_demo_age_grouped_condensed$CoB) %>%
    map(function(filter) est_inc_crude_adj(filter,
                                           pop = pop_age,
                                           cases = cases_age,
                                           rate_pop = 100000)) %>%
    bind_rows %>%
    rename(`Age group (condensed)` = cov) %>%
    mutate(`Age group (condensed)` =  `Age group (condensed)` %>%
             factor(levels = c( 'All cases (crude)', 'All cases (adj)',
                              '0-14', '15-64', '65+')),
           Year = as.numeric(Year))


  ## Munge case data and incidence data together

  cases_demo_incidence <- cases_demo %>%
    ungroup %>%
    mutate(Year = Year %>% as.character) %>%
    full_join(incidence) %>%
    mutate(Age = Age %>% as.factor)


  if (verbose) {
    ## Incidence by country of birth for single year of age
    incidence  %>%
      filter(as.numeric(Year) %% 4 == 0,
             !is.na(CoB), as.numeric(Year) < 2015,
             !(CoB %in% 'Total (LFS)')) %>%
      mutate(Year = ordered(Year)) %>%
      ggplot(aes(x = Age, y = Incidence, colour = Year)) +
      geom_pointrange(aes(ymin = Inc_LCI, ymax = Inc_UCI)) +
      facet_wrap(~ CoB, scales = 'free', nrow = 3) +
      theme_set() -> p

    interactive_plot(p, interactive)

    ## Incidence by country of birth grouped into 5 year age groups
    age_grouped_incidence %>%
      filter(as.numeric(Year) %% 2 == 0, !is.na(CoB), as.numeric(Year) < 2015, !(CoB %in% 'Total (LFS)')) %>%
      mutate(Year = ordered(Year)) %>%
      ggplot(aes(x = `Age group`, y = Incidence, colour = Year)) +
      geom_pointrange(aes(ymin = Inc_LCI, ymax = Inc_UCI), position = position_dodge(width = 0.2)) +
      facet_wrap(~ CoB, scales = 'free', nrow = 3) +
      theme_set() -> p2

    interactive_plot(p2, interactive)

    ## Just UK born for clarity
    age_grouped_incidence %>%
      filter(as.numeric(Year) %% 4 == 0, !is.na(CoB), as.numeric(Year) < 2015, CoB %in% 'UK born') %>%
      mutate(Year = ordered(Year)) %>%
      ggplot(aes(x = `Age group`, y = Incidence, colour = Year, fill = Year)) +
      geom_bar(position = 'dodge', stat = 'identity', alpha = 0.6) +
      geom_linerange(aes(ymin = Inc_LCI, ymax = Inc_UCI), position = position_dodge(width = 0.94)) +
      theme_set() -> p3

    interactive_plot(p3, interactive)

    ## By population, stratified by year
    age_grouped_incidence %>%
      filter(`Age group` %in% c('All cases (crude)', 'All cases (adj)')) %>%
      filter(!is.na(CoB), as.numeric(Year) < 2015, !(CoB %in% c('Total (LFS)', 'Total'))) %>%
      mutate(Year = ordered(Year)) %>%
      ggplot(aes(x = Year, y = Incidence, colour = `Age group`, fill = `Age group`)) +
      geom_pointrange(aes(ymin = Inc_LCI, ymax = Inc_UCI), position = position_dodge(width = 1)) +
      geom_line() +
      facet_wrap(~ CoB, scales = 'free', ncol = 3) +
      theme_set() -> p4

    interactive_plot(p4, interactive)
  }

  if (save) {
    save_data(incidence,
              name = incidence_name,
              path = save_path,
              format = save_format,
              message = "Incidence data saved to: ",
              verbose = verbose
    )

    save_data(grouped_incidence,
              name = grouped_incidence_name,
              path = save_path,
              format = save_format,
              message = "Grouped incidence data saved to: ",
              verbose = verbose
    )

    save_data(condensed_grouped_incidence,
              name = condensed_grouped_incidence_name,
              path = save_path,
              format = save_format,
              message = "Condensed grouped incidence data saved to: ",
              verbose = verbose
    )

    save_data(cases_demo_incidence,
              name = cases_demo_incidence_name,
              path = save_path,
              format = save_format,
              message = "Incidence and demographic data saved to: ",
              verbose = verbose
    )
  }

  if (return) {
    return(list(incidence,
                age_grouped_incidence,
                age_grouped_condensed_incidence,
                cases_demo_incidence))
  }
}

