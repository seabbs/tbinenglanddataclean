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
combine_ons_with_lfs <- function(data_path = "~/data/tbinenglanddataclean",
                                 ons_name = "E_demo_2000_2015.rds",
                                 lfs_name = "formatted_LFS_2000_2016.rds",
                                 countries = "England",
                                      return = TRUE,
                                      save = TRUE,
                                      save_name = "E_ons_lfs_2000_2016",
                                      save_path = "~/data/tbinenglanddataclean",
                                      verbose = TRUE,
                                      interactive = TRUE,
                                      theme = theme_minimal()) {


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
    summarise(Population = sum(Weight)) -> demo_2000_2016_strat_est



  # Format demographics for consistency -------------------------------------
  demo_2000_2015 %>%
    mutate(CoB = 'Total') %>%
    mutate(CoB = CoB) %>%
    mutate(Year = as.character(Year) %>% as.numeric) -> demo_2000_2015


  # Bind data ---------------------------------------------------------------
  demo_2000_2016_strat_est <- demo_2000_2015 %>%
    full_join(demo_2000_2016_strat_est, by = c('Year', 'Age', 'CoB', 'Population')) %>%
    mutate(CoB=factor(CoB, levels = c('Total', 'UK born', 'Non-UK born')))



  # Add comparison total from LFS -------------------------------------------
  demo_2000_2016_strat_est <- demo_2000_2016_strat_est %>%
    filter(!(CoB %in% 'Total')) %>%
    group_by(Age, Year) %>%
    summarise(Population = sum(Population)) %>%
    mutate(CoB = 'Total (LFS)') %>%
    bind_rows(demo_2000_2016_strat_est) %>%
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

  if (verbose) {
    ## Check breakdown
    table(demo_2000_2016_strat_est$`Age group (condensed)`, demo_2000_2016_strat_est$Age)
  }

  # Plots to visualise ------------------------------------------------------
 if (verbose) {
   ## Plots of Non-UK born over time
   demo_2000_2016_strat_est %>%
     filter(Year %% 5 == 0, CoB %in% 'Non-UK born') %>%
     ggplot(aes(x = Age, y = Population)) +
     geom_density(alpha = 0.4) +
     facet_wrap(~Year) +
     theme(axis.text.x = element_text(angle = 90)) -> p

   if (interactive) {
     print(ggplotly(p))
   }else {
     print(p)
   }

   ## Plots of UK born over time
   demo_2000_2016_strat_est %>%
     filter(Year %% 5 == 0, CoB %in% 'UK born') %>%
     ggplot(aes(x = Age, y = Population)) +
     geom_density(alpha = 0.4) +
     facet_wrap(~Year) +
     theme(axis.text.x = element_text(angle = 90)) -> p1

   if (interactive) {
     print(ggplotly(p1))
   }else {
     print(p1)
   }


   ## Compare Population strat by year - 2000
   demo_2000_2016_strat_est %>%
     filter(Year %in% 2000, !is.na(CoB)) %>%
     ggplot(aes(x = Age, y = Population)) +
     geom_density(alpha = 0.4) +
     facet_wrap(~CoB) +
     theme(axis.text.x = element_text(angle = 90)) -> p2

   if (interactive) {
     print(ggplotly(p2))
   }else {
     print(p2)
   }


   ## Compare Population strat by year - 2005
   demo_2000_2016_strat_est %>%
     filter(Year %in% 2005, !is.na(CoB)) %>%
     ggplot(aes(x = Age, y = Population)) +
     geom_density(alpha = 0.4) +
     facet_wrap(~CoB) +
     theme(axis.text.x = element_text(angle = 90)) -> p3

   if (interactive) {
     print(ggplotly(p3))
   }else {
     print(p3)
   }


   ## Compare Population strat by year - 2010
   demo_2000_2016_strat_est %>%
     filter(Year %in% 2010, !is.na(CoB)) %>%
     ggplot(aes(x = Age, y = Population)) +
     geom_density(alpha = 0.4) +
     facet_wrap(~CoB) +
     theme(axis.text.x = element_text(angle = 90)) -> p4

   if (interactive) {
     print(ggplotly(p4))
   }else {
     print(p4)
   }


   ## Compare Population strat by year - 2015
   demo_2000_2016_strat_est %>%
     filter(Year %in% 2015, !is.na(CoB)) %>%
     ggplot(aes(x = Age, y = Population)) +
     geom_density(alpha = 0.4) +
     facet_wrap(~CoB) +
     theme(axis.text.x = element_text(angle = 90)) -> p5

   if (interactive) {
     print(ggplotly(p5))
   }else {
     print(p5)
   }




   # Look at total population over time --------------------------------------
   demo_2000_2016_strat_est %>%
     filter(Year %% 5 == 0) %>%
     group_by(CoB, Year) %>%
     summarise(Population = sum(Population)) %>%
     ggplot(aes(x = Year, y = Population, fill = CoB, colour = CoB)) +
     geom_point() +
     geom_line() -> p6

   if (interactive) {
     print(ggplotly(p6))
   }else {
     print(p6)
   }
 }

  if (verbose) {
    #current bug in plotly for negative values in box plots means this will not present the correct results so use static table
    demo_2000_2016_strat_est %>%
      plot_pop_age_compare_ons_lfs -> p7
    print(p7)

    ## plot removing 85+ due to distortion
    demo_2000_2016_strat_est %>%
      filter(!(`Age group` %in% c('85-89', '90+'))) %>%
      plot_pop_age_compare_ons_lfs -> p8
    print(p8)

  }

  if (save) {
    save_file_path <- file.path(data_path, paste0(save_name, ".rds"))
    if (verbose) {
      message("ONS combined with LFS data saved to: ", save_file_path)
    }

    saveRDS(demo_2000_2016_strat_est, file = save_file_path)

  }

  if (return) {
    return(demo_2000_2016_strat_est)
  }
}
