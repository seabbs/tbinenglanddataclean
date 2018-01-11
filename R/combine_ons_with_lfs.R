#' A Function that Combines the ONS Demographic Data, with the LFS
#' Demographic Data
#'
#' @description This function takes demographic data summarised by \code{\link[tbinenglanddataclean]{clean_demographics_uk}} and
#' \code{\link[tbinenglanddataclean]{clean_labour_force_survey}} and combines it into a single tidy dataset.
#' Summary statistics and plots can be returned to check both datasets.
#' @inherit clean_demographics_uk
#' @param ons_name Character string of the file name of the ONS demographic data.
#' @param lfs_name Character string of the file name of the LFS demographic data.
#' @param countries  A character string, the countries to include in the
#' dataset. By default only England is included. Note this is reliant on the data being present in the
#' demographic datasets.
#'
#' @return A tidy tibble of demographic data by age between 2000 and 2015 for the specified countries
#' for both ONS and LFS data.
#' @export
#'
#' @examples
#'
combine_ons_with_lfs <- function(data_path = "~/data/tb_data/tbinenglanddataclean",
                                 ons_name = "E_demo_2000_2015.rds",
                                 lfs_name = "formatted_LFS_2000_2016.rds",
                                 countries = "England",
                                      return = TRUE,
                                      save = TRUE,
                                      save_name = "E_ons_lfs_2000_2016",
                                      save_path = "~/data/tb_data/tbinenglanddataclean",
                                      save_format = "rds",
                                      verbose = TRUE,
                                      interactive = TRUE,
                                      theme_set = NULL) {
  
  if (is.null(theme_set)) {
    theme_set <- theme_minimal()
  }

  demo_path <- file.path(data_path, ons_name)
  if (verbose) {
    message("Loading demographic data from: ", demo_path)
  }
  demo_2000_2015 <- readRDS(demo_path)

  lfs_path <- file.path(data_path, lfs_name)
  if (verbose) {
    message("Loading labour force survey data from: ", lfs_path)
  }
  lfs_data <- readRDS(lfs_path)


  # munge LFS to generate yearly population counts --------------------------

  lfs_data %>%
    filter(Country %in% countries) %>%
    group_by(Year, Age, CoB) %>%
    summarise(Population = sum(Weight)) %>%
    mutate(CoB = as.character(CoB)) -> demo_2000_2016_strat_est



  # Format demographics for consistency -------------------------------------
  demo_2000_2015 %>%
    mutate(CoB = 'Total') %>%
    mutate(CoB = CoB) %>%
    mutate(Year = as.character(Year) %>% as.numeric) -> demo_2000_2015


  # Bind data ---------------------------------------------------------------
  demo_2000_2016_strat_est <- demo_2000_2015 %>%
    full_join(demo_2000_2016_strat_est, by = c('Year', 'Age', 'CoB', 'Population')) %>%
    mutate(CoB = factor(CoB, levels = c('Total', 'UK born', 'Non-UK born')))



  # Add comparison total from LFS -------------------------------------------
  demo_2000_2016_strat_est <- demo_2000_2016_strat_est %>%
    filter(!(CoB %in% 'Total')) %>%
    group_by(Age, Year) %>%
    summarise(Population = sum(Population)) %>%
    mutate(CoB = 'Total (LFS)') %>%
    bind_rows(demo_2000_2016_strat_est %>%
                mutate(CoB = as.character(CoB))) %>%
    mutate(CoB = factor(CoB, levels = c('Total', 'Total (LFS)', 'UK born', 'Non-UK born')))



  # Add 5 year age groups ---------------------------------------------------
  demo_2000_2016_strat_est %>%
    mutate(`Age group` = Age %>%  as.character %>% replace(Age %in% '90+', '90') %>%
             as.numeric %>%
             cut(breaks = seq(0,95,5), right = FALSE,
                 ordered_result = TRUE,
                 labels = c(paste(seq(0,85,5), seq(4,89,5), sep = '-'), '90+'))) -> demo_2000_2016_strat_est

  ## Add 0-15, 16-65, 65+
  demo_2000_2016_strat_est %>%
    mutate(`Age group (condensed)` = Age %>%  as.character %>% replace(Age %in% '90+', '90') %>%
             as.numeric %>%
             cut(breaks = c(0, 15, 65, 91), right = FALSE,
                 ordered_result = TRUE, labels = c('0-14', '15-64', '65+'))) -> demo_2000_2016_strat_est

  ## ungroup
  demo_2000_2016_strat_est <- demo_2000_2016_strat_est %>%
    ungroup

  # Plots to visualise ------------------------------------------------------
 if (verbose) {
   ## Plots of Non-UK born over time
   demo_2000_2016_strat_est %>%
     filter(Year %% 5 == 0, CoB %in% 'Non-UK born') %>%
     ggplot(aes(x = Age, y = Population)) +
     geom_density(stat = "identity", alpha = 0.4) +
     facet_wrap(~Year) +
     theme_set +
     theme(axis.text.x = element_text(angle = 90)) +
     labs(caption = "Non-UK born population by age, every 5 years") -> p

   interactive_plot(p, interactive)

   ## Plots of UK born over time
   demo_2000_2016_strat_est %>%
     filter(Year %% 5 == 0, CoB %in% 'UK born') %>%
     ggplot(aes(x = Age, y = Population)) +
     geom_density(stat = "identity", alpha = 0.4) +
     facet_wrap(~Year) +
     theme_set +
     theme(axis.text.x = element_text(angle = 90)) +
     labs(caption = "UK born population by age, every 5 years") -> p1

   interactive_plot(p1, interactive)


   ## Compare Population strat by year - 2000
   demo_2000_2016_strat_est %>%
     filter(Year %in% 2000, !is.na(CoB)) %>%
     ggplot(aes(x = Age, y = Population)) +
     geom_density(stat = "identity", alpha = 0.4) +
     facet_wrap(~CoB) +
     theme_set +
     theme(axis.text.x = element_text(angle = 90)) +
     labs(caption = "Comparision of ONS, LFS, UK born,
          and non-UK born population estimates for 2000") -> p2

 interactive_plot(p2, interactive)


   ## Compare Population strat by year - 2005
   demo_2000_2016_strat_est %>%
     filter(Year %in% 2005, !is.na(CoB)) %>%
     ggplot(aes(x = Age, y = Population)) +
     geom_density(stat = "identity", alpha = 0.4) +
     facet_wrap(~CoB) +
     theme_set +
     theme(axis.text.x = element_text(angle = 90)) +
     labs(caption = "Comparision of ONS, LFS, UK born,
          and non-UK born population estimates for 2005") -> p3

   interactive_plot(p3, interactive)


   ## Compare Population strat by year - 2010
   demo_2000_2016_strat_est %>%
     filter(Year %in% 2010, !is.na(CoB)) %>%
     ggplot(aes(x = Age, y = Population)) +
     geom_density(stat = "identity", alpha = 0.4) +
     facet_wrap(~CoB) +
     theme_set +
     theme(axis.text.x = element_text(angle = 90)) +
     labs(caption = "Comparision of ONS, LFS, UK born,
          and non-UK born population estimates for 2010") -> p4

   interactive_plot(p4, interactive)


   ## Compare Population strat by year - 2015
   demo_2000_2016_strat_est %>%
     filter(Year %in% 2015, !is.na(CoB)) %>%
     ggplot(aes(x = Age, y = Population)) +
     geom_density(stat = "identity", alpha = 0.4) +
     facet_wrap(~CoB) +
     theme_set +
     theme(axis.text.x = element_text(angle = 90)) +
     labs(caption = "Comparision of ONS, LFS, UK born,
          and non-UK born population estimates for 2015") -> p5

   interactive_plot(p5, interactive)




   # Look at total population over time --------------------------------------
   demo_2000_2016_strat_est %>%
     filter(Year %% 5 == 0) %>%
     group_by(CoB, Year) %>%
     summarise(Population = sum(Population)) %>%
     ggplot(aes(x = Year, y = Population, fill = CoB, colour = CoB)) +
     geom_point() +
     geom_line() +
     theme_set +
     labs(caption = "Population estimates over time for both the ONS abd KFS") -> p6

   interactive_plot(p6, interactive)
 }

  if (verbose) {
    #current bug in plotly for negative values in box plots means this will not present the correct results so use static table
    demo_2000_2016_strat_est %>%
      na.omit %>%
      filter(Year < 2016) %>%
      plot_pop_age_compare_ons_lfs(theme_set = theme_set) -> p7
    interactive_plot(p7, interactive = FALSE)

    ## plot removing 85+ due to distortion
    demo_2000_2016_strat_est %>%
      filter (Year < 2016) %>%
      na.omit %>%
      filter(!(`Age group` %in% c('85-89', '90+'))) %>%
      plot_pop_age_compare_ons_lfs(theme_set = theme_set) -> p8
    interactive_plot(p8, interactive = FALSE)

  }

  if (save) {
    save_data(demo_2000_2016_strat_est,
              name = save_name,
              path = data_path,
              format = save_format,
              message = "ONS combined with LFS data saved to: ",
              verbose = verbose
    )
  }

  if (return) {
    return(demo_2000_2016_strat_est)
  }
}
